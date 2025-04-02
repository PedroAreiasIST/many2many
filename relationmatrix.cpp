#include "relationmatrix.hpp"
#include <map>
#include <utility>

size_t relationmatrix::nnode(size_t elementtype, size_t element, size_t nodetype)
{
    return getsize(operator()(elementtype, nodetype).nodesfromelement.lnods[element]);
}

size_t relationmatrix::nelem(size_t nodetype, size_t node, size_t elementtype)
{
    return getsize(operator()(elementtype, nodetype).elementsfromnode.lnods[node]);
}

void cleanmarked(relationmatrix &m) { erase(m.listofmarked); }
void marktoerase(relationmatrix &m, std::pair<size_t, size_t> const &node) { append(m.listofmarked, node); }
sek<std::pair<size_t, size_t>> getallelements(relationmatrix &m, std::pair<size_t, size_t> const &node)
{
    sek<std::pair<size_t, size_t>> ret;
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
            size_t element = m(elementtype, nodetype).elementsfromnode.lnods[nodenumber][localelement];
            ret[nret++] = std::make_pair(elementtype, element);
        }
    }
    setordered(ret);
    setunique(ret);
    return ret;
}

sek<std::pair<size_t, size_t>> getallnodes(relationmatrix &m, std::pair<size_t, size_t> const &element)
{
    sek<std::pair<size_t, size_t>> ret;
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
            size_t node = m(elementtype, nodetype).nodesfromelement.lnods[elementnumber][localnode];
            ret[nret++] = std::make_pair(nodetype, node);
        }
    }
    setordered(ret);
    setunique(ret);
    return ret;
}

sek<std::pair<size_t, size_t>> depthfirstsearchfromanode(relationmatrix &m, std::pair<size_t, size_t> const &node)
{
    using P = std::pair<size_t, size_t>;
    using SP = sek<P>;
    std::map<P, bool> visited;
    std::stack<P> stack;
    stack.push(node);
    sek<P> ret;
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

void setnumberoftypes(relationmatrix &m, size_t ntypes)
{
    m.ntypes = ntypes;
    setsize(m.m, ntypes * (ntypes + 1) / 2);
    setsize(m.groups, ntypes);
    for (size_t type = 0; type < ntypes; ++type)
    {
        setsize(m.groups[type], ntypes);
    }
}

void setsymmetrygroup(relationmatrix &m, size_t elementype, size_t nodetype, sek<sek<size_t>> const &group)
{
    m.groups[elementype][nodetype] = group;
}

size_t appendelement(relationmatrix &m, size_t elementype, size_t nodetype, sek<size_t> const &nodes)
{
    return appendelement(m(elementype, nodetype).nodesfromelement,
                         getcanonicalform(nodes, m.groups[elementype][nodetype]));
}
void setmany2many(relationmatrix &m, size_t elementype, size_t nodetype, many2many const &relation)
{
    m(elementype, nodetype) = relation;
}
many2many &getmany2many(relationmatrix &m, size_t elementype, size_t nodetype) { return m(elementype, nodetype); }

void lexiorder(relationmatrix &m, size_t elementype, size_t nodetype, sek<size_t> &order)
{
    lexiorder(m(elementype, nodetype), order);
}

void indicesfromorder(relationmatrix &m, size_t elementtype, size_t nodetype, sek<size_t> const &order,
                      sek<size_t> &oldfromnew, sek<size_t> &newfromold)
{
    indicesfromorder(m(elementtype, nodetype), order, oldfromnew, newfromold);
}

void compress(relationmatrix &m, size_t elementtype, sek<size_t> const &oldelementfromnew,
              sek<size_t> const &newelementfromold)
{
    for (size_t nodetype = 0; nodetype < m.ntypes; ++nodetype)
    {
        compresselements(m(elementtype, nodetype), oldelementfromnew);
    }
    // Note: the inner loop variable was shadowing the outer element type; renamed it to avoid confusion.
    for (size_t otherelementtype = 0; otherelementtype < m.ntypes; ++otherelementtype)
    {
        compressnodes(m(otherelementtype, elementtype), newelementfromold);
    }
}

void closeeverything(relationmatrix &m)
{
    for (size_t elementtype = 0; elementtype < m.ntypes; ++elementtype)
        for (size_t nodetype = 0; nodetype < m.ntypes; ++nodetype)
        {
            setallpointers(m(elementtype, nodetype));
        }
}

sek<size_t> getselementsfromnodes(relationmatrix &matrix, size_t elementtype, size_t nodestype,
                                  sek<size_t> const &nodes)
{
    return getelementsfromnodes(matrix(elementtype, nodestype),
                                getcanonicalform(nodes, matrix.groups[elementtype][nodestype]));
}
