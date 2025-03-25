//
// Created by pedro on 20-03-2025.
//
#include "relationmatrix.hpp"

void setnumberoftypes(relationmatrix &m, size_t ntypes)
{
    setsize(m.m, ntypes);
    for (size_t i = 0; i < ntypes; ++i)
    {
        setsize(m.m[i], ntypes);
    }
}

size_t appendelement(relationmatrix &m, size_t elementype, size_t nodetype, sek<size_t> const &nodes)
{
    return appendelement(m(elementype, nodetype).nodesfromelement, nodes);
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

void depthfirstsearch(relationmatrix &m, size_t startnodetype, size_t startnode, sek<size_t> &affectedelements,
                      sek<size_t> &affectedtypes)
{
    sek<size_t> startingtypes{getsize(m.m)};
    size_t nst = 0;
    for (size_t i = 0; i < getsize(m.m); ++i)
        if (m(i, startnodetype).elementsfromnode.nelem > 0)
        {
            startingtypes[nst++] = i;
        }
    for (size_t ist = 0; ist < nst; ++ist)
    {
    }
}

void compress(relationmatrix &m, size_t elementtype, sek<size_t> const &oldelementfromnew,
              sek<size_t> const &newelementfromold)
{
    for (size_t nodetype = 0; nodetype < getsize(m.m); ++nodetype)
    {
        compresselements(m(elementtype, nodetype), oldelementfromnew);
        for (size_t elementype = 0; elementtype < getsize(m.m); ++elementtype)
            compressnodes(m(nodetype, elementtype), newelementfromold);
    }
}

void closeelementnoderelation(relationmatrix &m, size_t elementype, size_t nodetype)
{
    setfromonetomany(m(elementype, nodetype));
}

sek<size_t> getselementsfromnodes(relationmatrix &matrix, size_t elementtype, size_t nodestype,
                                  sek<size_t> const &nodes)
{
    return getelementsfromnodes(matrix(elementtype, nodestype), nodes);
}

void getnodesfromanelement(relationmatrix &matrix, size_t elementtype, size_t element, size_t nodestype,
                           sek<size_t> &nodes)
{
    nodes = matrix(elementtype, nodestype).nodesfromelement.onetomany[element];
}

void getelementsfromnode(relationmatrix &matrix, size_t nodetype, size_t node, size_t elementstype,
                         sek<size_t> &elements)
{
    elements = matrix(elementstype, nodetype).elementsfromnode.onetomany[node];
}

void getnodes(relationmatrix &matrix, size_t type1, size_t element1, size_t type2, sek<size_t> &nodelist)
{
    sek<size_t> nodes1 = matrix(type1, type2).nodesfromelement.onetomany[element1];
    sek<size_t> nodes2 = matrix(type2, type1).elementsfromnode.onetomany[element1];
    nodelist = getunion(nodes1, nodes2);
    setunique(nodelist);
}

void getsallnodes(relationmatrix &matrix, size_t elementtype, size_t element, sek<sek<size_t>> &nodes)
{
    erase(nodes);
    setsize(nodes, getsize(matrix.m));
    for (size_t i = 0; i < getsize(matrix.m); ++i)
    {
        getnodesfromanelement(matrix, elementtype, element, i, nodes[i]);
    }
}
