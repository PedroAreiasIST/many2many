#include "many2many.hpp"

size_t many2many::nnodes(size_t element) { return getsize(nodesfromelement.lnods[element]); }
size_t many2many::nelems(size_t node) { return getsize(elementsfromnode.lnods[node]); }
void setnelem(many2many &rel, size_t nelem)
{
    rel.nodesfromelement.nelem = nelem;
    setsize(rel.nodesfromelement.lnods, nelem);
}
void setnnodes(many2many &rel, size_t element, size_t nnodes) { setsize(rel.nodesfromelement.lnods[element], nnodes); }
void setnodes(many2many &rel, size_t element, sequence<size_t> const &nodes)
{
    rel.nodesfromelement.lnods[element] = nodes;
}
size_t appendelement(many2many &rel, sequence<size_t> const &nodes)
{
    return appendelement(rel.nodesfromelement, nodes);
}
void multiplication(const many2many &rela, bool transposea, const many2many &relb, bool transposeb, many2many &relc)
{
    hidden::one2many const *a;
    hidden::one2many const *b;
    if (transposea)
    {
        a = &rela.elementsfromnode;
    } else
    {
        a = &rela.nodesfromelement;
    }
    if (transposeb)
    {
        b = &relb.elementsfromnode;
    } else
    {
        b = &relb.nodesfromelement;
    }
    multiplication(*a, *b, relc.nodesfromelement);
}
void addition(const many2many &rela, bool transposea, const many2many &relb, bool transposeb, many2many &relc)
{
    hidden::one2many const *a;
    hidden::one2many const *b;
    if (transposea)
    {
        a = &rela.elementsfromnode;
    } else
    {
        a = &rela.nodesfromelement;
    }
    if (transposeb)
    {
        b = &relb.elementsfromnode;
    } else
    {
        b = &relb.nodesfromelement;
    }
    addition(*a, *b, relc.nodesfromelement);
}
void intersection(const many2many &rela, bool transposea, const many2many &relb, bool transposeb, many2many &relc)
{
    hidden::one2many const *a;
    hidden::one2many const *b;
    if (transposea)
    {
        a = &rela.elementsfromnode;
    } else
    {
        a = &rela.nodesfromelement;
    }
    if (transposeb)
    {
        b = &relb.elementsfromnode;
    } else
    {
        b = &relb.nodesfromelement;
    }
    intersection(*a, *b, relc.nodesfromelement);
}
void difference(const many2many &rela, bool transposea, const many2many &relb, bool transposeb, many2many &relc)
{
    hidden::one2many const *a;
    hidden::one2many const *b;
    if (transposea)
    {
        a = &rela.elementsfromnode;
    } else
    {
        a = &rela.nodesfromelement;
    }
    if (transposeb)
    {
        b = &relb.elementsfromnode;
    } else
    {
        b = &relb.nodesfromelement;
    }
    difference(*a, *b, relc.nodesfromelement);
}

void setallpointers(many2many &rel)
{
    transpose(rel.nodesfromelement, rel.elementsfromnode);
    setsize(rel.nodelocation, rel.elementsfromnode.nelem);
    for (size_t localnode = 0; localnode < rel.elementsfromnode.nelem; ++localnode)
    {
        setsize(rel.nodelocation[localnode], getsize(rel.elementsfromnode.lnods[localnode]));
    }
    setsize(rel.elementlocation, rel.nodesfromelement.nelem);
    for (size_t localelement = 0; localelement < rel.nodesfromelement.nelem; ++localelement)
    {
        setsize(rel.elementlocation[localelement], getsize(rel.nodesfromelement.lnods[localelement]));
    }
    hidden::lst nextlocalelementofnode;
    setsize(nextlocalelementofnode, rel.elementsfromnode.nelem);
    for (size_t localelement = 0, elementCount = getsize(rel.nodesfromelement.lnods); localelement < elementCount;
         ++localelement)
    {
        const hidden::lst &nodes = rel.nodesfromelement.lnods[localelement];
        for (size_t localnode = 0, nodeCount = getsize(nodes); localnode < nodeCount; ++localnode)
        {
            size_t node = nodes[localnode];
            rel.nodelocation[node][nextlocalelementofnode[node]] = localnode;
            nextlocalelementofnode[node]++;
        }
    }
    hidden::lst nextlocalnodeofelement;
    setsize(nextlocalnodeofelement, rel.nodesfromelement.nelem);
    for (size_t localnode = 0, nodeCount = getsize(rel.elementsfromnode.lnods); localnode < nodeCount; ++localnode)
    {
        const hidden::lst &elements = rel.elementsfromnode.lnods[localnode];
        for (size_t localelement = 0, elemCount = getsize(elements); localelement < elemCount; ++localelement)
        {
            size_t element = elements[localelement];
            rel.elementlocation[element][nextlocalnodeofelement[element]] = localelement;
            nextlocalnodeofelement[element]++;
        }
    }
}
hidden::lst getelementsfromnodes(many2many const &rel, hidden::lst const &nodes)
{
    sequence<size_t> elems;
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
        if (j == bestNodeIndex)
            continue;
        elems = getintersection(elems, rel.elementsfromnode.lnods[nodes[j]]);
    }
    return elems;
}
hidden::lst getneighbours(many2many const &rel, size_t element)
{
    hidden::lst neighbours;
    setsize(neighbours, 0);
    const hidden::lst &elementNodes = rel.nodesfromelement.lnods[element];
    for (size_t nodePos = 0, nodeCount = getsize(elementNodes); nodePos < nodeCount; ++nodePos)
    {
        size_t node = elementNodes[nodePos];
        const hidden::lst &nodeElements = rel.elementsfromnode.lnods[node];
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
void indicesfromorder(many2many const &rel, const hidden::lst &elementorder, hidden::lst &oldfromnew,
                      hidden::lst &newfromold)
{
    indicesfromorder(rel.nodesfromelement, elementorder, oldfromnew, newfromold);
}
void compresselements(many2many &rel, hidden::lst const &oldelementfromnew)
{
    compresselements(rel.nodesfromelement, oldelementfromnew);
    setallpointers(rel);
}
void compressnodes(many2many &rel, hidden::lst const &newnodefromold)
{
    compressnodes(rel.nodesfromelement, newnodefromold);
    setallpointers(rel);
}
void getelementstoelements(many2many const &rel, many2many &elementstoelements)
{
    multiplication(rel.nodesfromelement, rel.elementsfromnode, elementstoelements.nodesfromelement);
    setallpointers(elementstoelements);
}
void getnodestonodes(many2many const &rel, many2many &nodestonodes)
{
    multiplication(rel.elementsfromnode, rel.nodesfromelement, nodestonodes.nodesfromelement);
    setallpointers(nodestonodes);
}
void lexiorder(many2many const &rel, hidden::lst &orderofelements) { lexiorder(rel.nodesfromelement, orderofelements); }
void toporder(many2many const &rel, bool transpose, hidden::lst &order)
{
    if (!transpose)
    {
        toporder(rel.nodesfromelement, order);
    } else
    {
        toporder(rel.elementsfromnode, order);
    }
}
size_t getlocalnodeposition(many2many const &rel, size_t node, size_t localelement)
{
    return rel.nodelocation[node][localelement];
}
size_t getlocalelementposition(many2many const &rel, size_t element, size_t localnode)
{
    return rel.elementlocation[element][localnode];
}
