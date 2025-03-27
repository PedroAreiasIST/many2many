// Created by pedro on 9/18/24.
#include "relmanytomany.hpp"

size_t relmanytomany::nnodes(size_t element) { return getsize(nodesfromelement.lnods[element]); }

size_t relmanytomany::nelems(size_t node) { return getsize(elementsfromnode.lnods[node]); }

void setfromonetomany(relmanytomany &rel)
{
    // Transpose the element-to-node mapping into node-to-element mapping.
    transpose(rel.nodesfromelement, rel.elementsfromnode);

    // Resize the local indices array for each node.
    setsize(rel.locn, rel.elementsfromnode.nelems);
    for (size_t node = 0; node < rel.elementsfromnode.nelems; ++node)
    {
        setsize(rel.locn[node], getsize(rel.elementsfromnode.lnods[node]));
    }

    // Initialize a counter for each node.
    lst nextIndex;
    setsize(nextIndex, rel.elementsfromnode.nelems);

    // Record local indices for each node for every element.
    for (size_t element = 0, elementCount = getsize(rel.nodesfromelement.lnods); element < elementCount; ++element)
    {
        for (size_t nodePos = 0, nodeCount = getsize(rel.nodesfromelement.lnods[element]); nodePos < nodeCount;
             ++nodePos)
        {
            size_t node = rel.nodesfromelement.lnods[element][nodePos];
            rel.locn[node][nextIndex[node]] = nodePos;
            nextIndex[node]++; // increment the local index counter for this node
        }
    }
}

lst getelementsfromnodes(relmanytomany const &rel, lst const &nodes)
{
    lst elems;
    if (getsize(nodes) > 0)
    {
        // Start with the element list of the first node
        elems = rel.elementsfromnode.lnods[nodes[0]];
        // Intersect with element lists of all subsequent nodes
        for (size_t i = 1, n = getsize(nodes); i < n; ++i)
        {
            elems = getintersection(elems, rel.elementsfromnode.lnods[nodes[i]]);
        }
    }
    return elems;
}

lst getneighbours(relmanytomany const &rel, size_t element)
{
    lst neighbours;
    size_t neighbourCount = 0;
    // First pass: count the total number of neighboring elements.
    for (size_t nodePos = 0, nodeCount = getsize(rel.nodesfromelement.lnods[element]); nodePos < nodeCount; ++nodePos)
    {
        size_t node = rel.nodesfromelement.lnods[element][nodePos];
        for (size_t elemPos = 0, elemCount = getsize(rel.elementsfromnode.lnods[node]); elemPos < elemCount; ++elemPos)
        {
            if (rel.elementsfromnode.lnods[node][elemPos] != element)
            {
                neighbourCount++;
            }
        }
    }
    setsize(neighbours, neighbourCount);

    // Second pass: collect all neighboring elements.
    neighbourCount = 0;
    for (size_t nodePos = 0, nodeCount = getsize(rel.nodesfromelement.lnods[element]); nodePos < nodeCount; ++nodePos)
    {
        size_t node = rel.nodesfromelement.lnods[element][nodePos];
        for (size_t elemPos = 0, elemCount = getsize(rel.elementsfromnode.lnods[node]); elemPos < elemCount; ++elemPos)
        {
            size_t otherElem = rel.elementsfromnode.lnods[node][elemPos];
            if (otherElem != element)
            {
                neighbours[neighbourCount++] = otherElem;
            }
        }
    }
    setordered(neighbours);
    setunique(neighbours);
    return neighbours;
}

void indicesfromorder(relmanytomany const &rel, const lst &elementorder, lst &oldfromnew, lst &newfromold)
{
    // Forward to the one-to-many version of indicesfromorder
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
    // Generate a lexicographical order of elements using the one-to-many relation
    toporder(rel.nodesfromelement, orderofelements);
}

void toporder(relmanytomany const &rel, bool transpose, lst &order)
{
    if (!transpose)
    {
        toporder(rel.nodesfromelement, order);
    } else
    {
        toporder(rel.elementsfromnode, order);
    }
}

size_t getlocalnodeposition(relmanytomany const &rel, size_t node, size_t localelement)
{
    return rel.locn[node][localelement];
}
