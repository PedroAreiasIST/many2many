#include "mm2m.hpp"
#include <set>
#include <stack>
#include <utility>

int mm2m::nnodes(int elementtype, int element, int nodetype)
{
    return getsize(operator()(elementtype, nodetype)
        .e2n.lnods[element]);
}

int mm2m::nelems(int nodetype, int node, int elementtype)
{
    return getsize(operator()(elementtype, nodetype)
        .n2e.lnods[node]);
}

void resetmarked(mm2m &m) { erase(m.listofmarked); }

void marktoerase(mm2m &m, std::pair<int, int> const &node)
{
    append(m.listofmarked, node);
}

seque<std::pair<int, int> > getallelements(mm2m &m,
                                           std::pair<int, int> const &node)
{
    seque<std::pair<int, int> > ret;
    auto [nodetype, nodenumber] = node;
    int nret = std::accumulate(
        m.m.begin(), m.m.end(), 0, [&](int sum, const auto &elementtype)
        {
            return sum + m.nelems(nodetype, nodenumber, &elementtype - &m.m[0]);
        });

    setsize(ret, nret);
    nret = 0;
    // Second pass: collect (element type, element number) pairs.
    for (int elementtype = 0; elementtype < m.ntypes; ++elementtype)
    {
        const int numberofelements = m.nelems(nodetype, nodenumber, elementtype);
        for (int localelement = 0; localelement < numberofelements;
             ++localelement)
        {
            ret[nret++] = std::make_pair(
                elementtype, m(elementtype, nodetype)
                .n2e.lnods[nodenumber][localelement]);
        }
    }
    setorderedandunique(ret);
    return ret;
}

seque<std::pair<int, int> > getallnodes(mm2m &m,
                                        std::pair<int, int> const &element)
{
    seque<std::pair<int, int> > ret;
    auto [elementtype, elementnumber] = element;
    int nret =
            std::accumulate(m.m.begin(), m.m.end(), 0, [&](int sum, const auto &)
            {
                return sum + m.nnodes(elementtype, elementnumber, &m.m[0] - &m.m[0]);
            });
    setsize(ret, nret);
    nret = 0;

    // Second pass: collect (node type, node number) pairs.
    for (int nodetype = 0; nodetype < m.ntypes; ++nodetype)
    {
        const int numberofnodes = m.nnodes(elementtype, elementnumber, nodetype);
        for (int localnode = 0; localnode < numberofnodes; ++localnode)
        {
            ret[nret++] = std::make_pair(
                nodetype, m(elementtype, nodetype)
                .e2n.lnods[elementnumber][localnode]);
        }
    }
    setorderedandunique(ret);
    return ret;
}

seque<std::pair<int, int> >
depthfirstsearchfromanode(mm2m &m, std::pair<int, int> const &node)
{
    using P = std::pair<int, int>;
    seque<P> ret;
    std::set<P> visited;
    std::stack<P> stack;
    stack.push(node);
    while (!stack.empty())
    {
        auto current = stack.top();
        stack.pop();
        if (visited.find(current) == visited.end())
        {
            visited.insert(current);
            append(ret, current);
            auto const &elements = getallelements(m, current);
            for (int i = 0; i < getsize(elements); ++i)
            {
                if (visited.find(elements[i]) == visited.end())
                    stack.push(elements[i]);
            }
        }
    }
    setorderedandunique(ret);
    return ret;
}

void setnumberoftypes(mm2m &m, int ntypes)
{
    m.ntypes = ntypes;
    setsize(m.m, ntypes);
    for (int type = 0; type < ntypes; ++type)
    {
        setsize(m.m[type], ntypes);
    }
    setsize(m.groups, ntypes);
    for (int type = 0; type < ntypes; ++type)
    {
        setsize(m.groups[type], ntypes);
    }
}

void setsymmetrygroup(mm2m &m, int elementype, int nodetype,
                      seque<seque<int> > const &group)
{
    m.groups[elementype][nodetype] = group;
}

int appendelement(mm2m &m, int elementype, int nodetype,
                  seque<int> const &nodes)
{
    return appendelement(m(elementype, nodetype),
                         getcanonicalform(nodes, m.groups[elementype][nodetype]));
}

int appendelement(mm2m &m, int elementype)
{
    auto newn = getsize(m(elementype, elementype).e2n.lnods);
    return appendelement(m(elementype, elementype), {newn});
}

void setmany2many(mm2m &m, int elementype, int nodetype, m2m const &relation)
{
    m(elementype, nodetype) = relation;
}

m2m &getmany2many(mm2m &m, int elementype, int nodetype)
{
    return m(elementype, nodetype);
}

void indicesfromorder(mm2m &m, int elementtype, int nodetype,
                      seque<int> const &order, seque<int> &oldfromnew,
                      seque<int> &newfromold)
{
}

void compress(mm2m &m, int elementtype, seque<int> const &oldelementfromnew,
              seque<int> const &newelementfromold)
{
    for (int nodetype = 0; nodetype < m.ntypes; ++nodetype)
    {
        compresselements(m(elementtype, nodetype), oldelementfromnew);
    }
    // Note: the inner loop variable was shadowing the outer element type; renamed
    // it to avoid confusion.
    for (int otherelementtype = 0; otherelementtype < m.ntypes;
         ++otherelementtype)
    {
        permutenodes(m(otherelementtype, elementtype), newelementfromold);
    }
}

void compress(mm2m &m)
{
    setordered(m.listofmarked);
    setorderedandunique(m.listofmarked);
    for (int counter = 0; counter < getsize(m.listofmarked); ++counter)
    {
        append(m.listofmarked,
               depthfirstsearchfromanode(m, m.listofmarked[counter]));
    }
    setordered(m.listofmarked);
    setunique(m.listofmarked);
    seque<seque<int> > nodes(m.ntypes);
    seque<int> marker(m.ntypes, 0);
    for (int counter = 0; counter < getsize(m.listofmarked); ++counter)
    {
        append(nodes[m.listofmarked[counter].first],
               m.listofmarked[counter].second);
    }
    for (int type = 0; type < m.ntypes; ++type)
    {
        int maxn = 0;
        for (int elementtypes = 0; elementtypes < m.ntypes; ++elementtypes)
        {
            maxn = std::max(maxn, m(type, elementtypes).e2n.nelem);
            maxn = std::max(maxn, m(elementtypes, type).n2e.nelem);
        }
        seque<bool> ismarked(maxn, false);
        for (int pos = 0; pos < getsize(nodes[type]); ++pos)
        {
            ismarked[nodes[type][pos]] = true;
        }
        seque<int> oldfromnew(maxn);
        seque<int> newfromold(maxn);
        int k = 0;
        for (int i = 0; i < maxn; ++i)
        {
            if (!ismarked[i])
            {
                oldfromnew[k] = i;
                newfromold[i] = k;
                k++;
            }
            compress(m, type, oldfromnew, newfromold);
        }
    }
}

seque<int> typetoporder(mm2m const &m)
{
    o2m typedeps;
    setsize(typedeps, m.ntypes);
    for (int elementtype = 0; elementtype < m.ntypes; ++elementtype)
    {
        for (int nodetype = 0; nodetype < m.ntypes; ++nodetype)
        {
            if (nodetype != elementtype)
            {
                if (m(nodetype, elementtype).e2n.nelem != 0)
                {
                    append(typedeps.lnods[elementtype], nodetype);
                }
            }
        }
    }
    return toporder(typedeps);
}

void closeeverything(mm2m &m)
{
    for (int elementtype = 0; elementtype < m.ntypes; ++elementtype)
        for (int nodetype = 0; nodetype < m.ntypes; ++nodetype)
        {
            setallpointers(m(elementtype, nodetype));
        }
}

seque<int> getselementsfromnodes(mm2m &matrix, int elementtype, int nodestype,
                                 seque<int> const &nodes)
{
    return getelementsfromnodes(
        matrix(elementtype, nodestype),
        getcanonicalform(nodes, matrix.groups[elementtype][nodestype]));
}
