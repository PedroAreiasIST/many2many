#include "mm2m.hpp"
#include <map>
#include <utility>

size_t mm2m::nnode(size_t elementtype, size_t element, size_t nodetype)
{
    return getsize(operator()(elementtype, nodetype).nodesfromelement.lnods[element]);
}

size_t mm2m::nelem(size_t nodetype, size_t node, size_t elementtype)
{
    return getsize(operator()(elementtype, nodetype).elementsfromnode.lnods[node]);
}

void resetmarked(mm2m &m) { erase(m.listofmarked); }
void marktoerase(mm2m &m, std::pair<size_t, size_t> const &node) { append(m.listofmarked, node); }
seque<std::pair<size_t, size_t>> getallelements(mm2m &m, std::pair<size_t, size_t> const &node)
{
    seque<std::pair<size_t, size_t>> ret;
    auto [nodetype, nodenumber] = node;
    size_t nret = 0;

    // First pass: count total elements.
    for (size_t elementtype = 0; elementtype < m.ntypes; ++elementtype)
    {
        nret += m.nelem(nodetype, nodenumber, elementtype);
    }

    setsize(ret, nret);
    nret = 0;

    // Second pass: collect (element type, element number) pairs.
    for (size_t elementtype = 0; elementtype < m.ntypes; ++elementtype)
    {
        const size_t numberofelements = m.nelem(nodetype, nodenumber, elementtype);
        for (size_t localelement = 0; localelement < numberofelements; ++localelement)
        {
            ret[nret++] = std::make_pair(elementtype,
                                         m(elementtype, nodetype).elementsfromnode.lnods[nodenumber][localelement]);
        }
    }
    setordered(ret);
    setunique(ret);
    return ret;
}

seque<std::pair<size_t, size_t>> getallnodes(mm2m &m, std::pair<size_t, size_t> const &element)
{
    seque<std::pair<size_t, size_t>> ret;
    auto [elementtype, elementnumber] = element;
    size_t nret = 0;

    // First pass: count total nodes.
    for (size_t nodetype = 0; nodetype < m.ntypes; ++nodetype)
    {
        nret += m.nnode(elementtype, elementnumber, nodetype);
    }
    setsize(ret, nret);
    nret = 0;

    // Second pass: collect (node type, node number) pairs.
    for (size_t nodetype = 0; nodetype < m.ntypes; ++nodetype)
    {
        const size_t numberofnodes = m.nnode(elementtype, elementnumber, nodetype);
        for (size_t localnode = 0; localnode < numberofnodes; ++localnode)
        {
            ret[nret++] =
                    std::make_pair(nodetype, m(elementtype, nodetype).nodesfromelement.lnods[elementnumber][localnode]);
        }
    }
    setordered(ret);
    setunique(ret);
    return ret;
}

seque<std::pair<size_t, size_t>> depthfirstsearchfromanode(mm2m &m, std::pair<size_t, size_t> const &node)
{
    using P = std::pair<size_t, size_t>;
    using SP = seque<P>;
    std::map<P, bool> visited;
    std::stack<P> stack;
    stack.push(node);
    seque<P> ret;
    while (!stack.empty())
    {
        P current = stack.top();
        stack.pop();
        if (!visited[current])
        {
            visited[current] = true;
            append(ret, current);
            SP elements = getallelements(m, current);
            for (size_t i = 0; i < getsize(elements); ++i)
            {
                if (!visited[elements[i]])
                    stack.push(elements[i]);
            }
        }
    }
    setordered(ret);
    setunique(ret);
    return ret;
}

void setnumberoftypes(mm2m &m, size_t ntypes)
{
    m.ntypes = ntypes;
    setsize(m.m, ntypes);
    for (size_t type = 0; type < ntypes; ++type)
    {
        setsize(m.m[type], ntypes);
    }
    setsize(m.groups, ntypes);
    for (size_t type = 0; type < ntypes; ++type)
    {
        setsize(m.groups[type], ntypes);
    }
}

void setsymmetrygroup(mm2m &m, size_t elementype, size_t nodetype, seque<seque<size_t>> const &group)
{
    m.groups[elementype][nodetype] = group;
}

size_t appendelement(mm2m &m, size_t elementype, size_t nodetype, seque<size_t> const &nodes)
{
    return appendelement(m(elementype, nodetype).nodesfromelement,
                         getcanonicalform(nodes, m.groups[elementype][nodetype]));
}
void setmany2many(mm2m &m, size_t elementype, size_t nodetype, m2m const &relation)
{
    m(elementype, nodetype) = relation;
}
m2m &getmany2many(mm2m &m, size_t elementype, size_t nodetype) { return m(elementype, nodetype); }

void indicesfromorder(mm2m &m, size_t elementtype, size_t nodetype, seque<size_t> const &order,
                      seque<size_t> &oldfromnew, seque<size_t> &newfromold)
{
    indicesfromorder(m(elementtype, nodetype), order, oldfromnew, newfromold);
}

void compress(mm2m &m, size_t elementtype, seque<size_t> const &oldelementfromnew,
              seque<size_t> const &newelementfromold)
{
    for (size_t nodetype = 0; nodetype < m.ntypes; ++nodetype)
    {
        compresselements(m(elementtype, nodetype), oldelementfromnew);
    }
    // Note: the inner loop variable was shadowing the outer element type; renamed it to avoid confusion.
    for (size_t otherelementtype = 0; otherelementtype < m.ntypes; ++otherelementtype)
    {
        permutenodes(m(otherelementtype, elementtype), newelementfromold);
    }
}

void compress(mm2m &m)
{
    setordered(m.listofmarked);
    setorderedandunique(m.listofmarked);
    for (size_t counter = 0; counter < getsize(m.listofmarked); ++counter)
    {
        append(m.listofmarked, depthfirstsearchfromanode(m, m.listofmarked[counter]));
    }
    setordered(m.listofmarked);
    setunique(m.listofmarked);
    seque<seque<size_t>> nodes(m.ntypes);
    seque<size_t> marker(m.ntypes, 0);
    for (size_t counter = 0; counter < getsize(m.listofmarked); ++counter)
    {
        append(nodes[m.listofmarked[counter].first], m.listofmarked[counter].second);
    }
    for (size_t type = 0; type < m.ntypes; ++type)
    {
        size_t maxn = 0;
        for (size_t elementtypes = 0; elementtypes < m.ntypes; ++elementtypes)
        {
            maxn = std::max(maxn, m(type, elementtypes).nodesfromelement.nelem);
            maxn = std::max(maxn, m(elementtypes, type).elementsfromnode.nelem);
        }
        seque<bool> ismarked(maxn, false);
        for (size_t pos = 0; pos < getsize(nodes[type]); ++pos)
        {
            ismarked[nodes[type][pos]] = true;
        }
        seque<size_t> oldfromnew(maxn);
        seque<size_t> newfromold(maxn);
        size_t k = 0;
        for (size_t i = 0; i < maxn; ++i)
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
void typetoporder(mm2m const &m, seque<size_t> &order)
{
    hidden::one2many typedeps;
    setnelem(typedeps, m.ntypes);
    for (size_t elementtype = 0; elementtype < m.ntypes; ++elementtype)
    {
        for (size_t nodetype = 0; nodetype < m.ntypes; ++nodetype)
            if (m(elementtype, nodetype).nodesfromelement.nelem != 0)
            {
                append(typedeps.lnods[elementtype], nodetype);
            }
    }
    toporder(typedeps);
}

void closeeverything(mm2m &m)
{
    for (size_t elementtype = 0; elementtype < m.ntypes; ++elementtype)
        for (size_t nodetype = 0; nodetype < m.ntypes; ++nodetype)
        {
            setallpointers(m(elementtype, nodetype));
        }
}

seque<size_t> getselementsfromnodes(mm2m &matrix, size_t elementtype, size_t nodestype, seque<size_t> const &nodes)
{
    return getelementsfromnodes(matrix(elementtype, nodestype),
                                getcanonicalform(nodes, matrix.groups[elementtype][nodestype]));
}
