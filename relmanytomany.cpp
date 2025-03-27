#include "relmanytomany.hpp"

size_t relmanytomany::nnodes(size_t element) { return getsize(nodesfromelement.lnods[element]); }
size_t relmanytomany::nelems(size_t node) { return getsize(elementsfromnode.lnods[node]); }

void setfromonetomany(relmanytomany &rel)
{
    transpose(rel.nodesfromelement, rel.elementsfromnode);
    setsize(rel.locn, rel.elementsfromnode.nelems);
    for (size_t node = 0; node < rel.elementsfromnode.nelems; ++node)
    {
        setsize(rel.locn[node], getsize(rel.elementsfromnode.lnods[node]));
    }

    lst nextIndex;
    setsize(nextIndex, rel.elementsfromnode.nelems);
    for (size_t element = 0, elementCount = getsize(rel.nodesfromelement.lnods); element < elementCount; ++element)
    {
        const lst &nodes = rel.nodesfromelement.lnods[element];
        for (size_t nodePos = 0, nodeCount = getsize(nodes); nodePos < nodeCount; ++nodePos)
        {
            size_t node = nodes[nodePos];
            rel.locn[node][nextIndex[node]] = nodePos;
            nextIndex[node]++;
        }
    }
}

lst getelementsfromnodes(relmanytomany const &rel, lst const &nodes)
{
    lst elems;
    if (getsize(nodes) == 0)
    {
        return elems;
    }
    size_t bestNodeIndex = 0;
    size_t minSize = getsize(rel.elementsfromnode.lnods[nodes[0]]);
    for (size_t j = 1, n = getsize(nodes); j < n; ++j)
    {
        size_t node = nodes[j];
        size_t size_j = getsize(rel.elementsfromnode.lnods[node]);
        if (size_j < minSize)
        {
            minSize = size_j;
            bestNodeIndex = j;
        }
    }
    elems = rel.elementsfromnode.lnods[nodes[bestNodeIndex]];
    for (size_t j = 0, n = getsize(nodes); j < n; ++j)
    {
        if (j == bestNodeIndex) continue;
        elems = getintersection(elems, rel.elementsfromnode.lnods[nodes[j]]);
    }
    return elems;
}

lst getneighbours(relmanytomany const &rel, size_t element)
{
    lst neighbours;
    setsize(neighbours, 0);
    const lst &elementNodes = rel.nodesfromelement.lnods[element];
    for (size_t nodePos = 0, nodeCount = getsize(elementNodes); nodePos < nodeCount; ++nodePos)
    {
        size_t node = elementNodes[nodePos];
        const lst &nodeElements = rel.elementsfromnode.lnods[node];
        for (size_t elemPos = 0, elemCount = getsize(nodeElements); elemPos < elemCount; ++elemPos)
        {
            size_t otherElem = nodeElements[elemPos];
            if (otherElem != element)
            {
                append(neighbours, otherElem);
            }
        }
    }
    setordered(neighbours);
    setunique(neighbours);
    return neighbours;
}

void indicesfromorder(relmanytomany const &rel, const lst &elementorder, lst &oldfromnew, lst &newfromold)
{
    indicesfromorder(rel.nodesfromelement, elementorder, oldfromnew, newfromold);
}

void compresselements(relmanytomany &rel, lst const &oldelementfromnew)
{
    compresselements(rel.nodesfromelement, oldelementfromnew);
    setfromonetomany(rel);
}

void compressnodes(relmanytomany &rel, lst const &newnodefromold)
{
    compressnodes(rel.nodesfromelement, newnodefromold);
    setfromonetomany(rel);
}

void lexiorder(relmanytomany const &rel, lst &orderofelements)
{
    lexiorder(rel.nodesfromelement, orderofelements);
}

void toporder(relmanytomany const &rel, bool transpose, lst &order)
{
    if (!transpose) {
        toporder(rel.nodesfromelement, order);
    } else {
        toporder(rel.elementsfromnode, order);
    }
}

size_t getlocalnodeposition(relmanytomany const &rel, size_t node, size_t localelement)
{
    return rel.locn[node][localelement];
}
