#include "relationmatrix.hpp"
#include <map>
#include <utility>

size_t relationmatrix::nnode(size_t elementtype, size_t element, size_t nodetype)
{
    return getsize(m[elementtype][nodetype].nodesfromelement.lnods[element]);
}

size_t relationmatrix::nelem(size_t nodetype, size_t node, size_t elementtype)
{
    return getsize(m[elementtype][nodetype].elementsfromnode.lnods[node]);
}

sek<std::pair<size_t, size_t>> getallelements(relationmatrix &m, std::pair<size_t, size_t> const &node)
{
    sek<std::pair<size_t, size_t>> ret;
    auto [nodetype, nodenumber] = node;
    size_t nret = 0;

    // First pass: count total elements.
    for (size_t elementtype = 0; elementtype < getsize(m.m); ++elementtype)
    {
        nret += m.nelem(nodetype, nodenumber, elementtype);
    }

    setsize(ret, nret);
    nret = 0;

    // Second pass: collect (element type, element number) pairs.
    for (size_t elementtype = 0; elementtype < getsize(m.m); ++elementtype)
    {
        const size_t numElems = m.nelem(nodetype, nodenumber, elementtype);
        for (size_t localelement = 0; localelement < numElems; ++localelement)
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
    for (size_t nodetype = 0; nodetype < getsize(m.m); ++nodetype)
    {
        nret += m.nnode(elementtype, elementnumber, nodetype);
    }
    setsize(ret, nret);
    nret = 0;

    // Second pass: collect (node type, node number) pairs.
    for (size_t nodetype = 0; nodetype < getsize(m.m); ++nodetype)
    {
        const size_t numNodes = m.nnode(elementtype, elementnumber, nodetype);
        for (size_t localnode = 0; localnode < numNodes; ++localnode)
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

sek<std::pair<size_t, size_t>> depthfirstsearch(relationmatrix &m, size_t node)
{
    sek<std::pair<size_t, size_t>> ret;
    for (size_t nodetype = 0; nodetype < getsize(m.m); ++nodetype)
    {
        ret = getunion(ret, depthfirstsearchfromanode(m, std::make_pair(nodetype, node)));
    }
    setordered(ret);
    setunique(ret);
    return ret;
}

void setnumberoftypes(relationmatrix &m, size_t ntypes)
{
    setsize(m.m, ntypes);
    for (size_t i = 0; i < ntypes; ++i)
    {
        setsize(m.m[i], ntypes);
    }
    setsize(m.groups, ntypes);
    for (size_t i = 0; i < ntypes; ++i)
    {
        setsize(m.groups[i], ntypes);
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
    for (size_t nodetype = 0; nodetype < getsize(m.m); ++nodetype)
    {
        compresselements(m(elementtype, nodetype), oldelementfromnew);
        // Note: the inner loop variable was shadowing the outer element type; renamed it to avoid confusion.
        for (size_t et = 0; et < getsize(m.m); ++et)
        {
            compressnodes(m(nodetype, et), newelementfromold);
        }
    }
}

void closeelementnoderelation(relationmatrix &m, size_t elementype, size_t nodetype)
{
    setfromonetomany(m(elementype, nodetype));
}

sek<size_t> getselementsfromnodes(relationmatrix &matrix, size_t elementtype, size_t nodestype,
                                  sek<size_t> const &nodes)
{
    return getelementsfromnodes(matrix(elementtype, nodestype),
                                getcanonicalform(nodes, matrix.groups[elementtype][nodestype]));
}
