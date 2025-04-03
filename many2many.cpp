#include "many2many.hpp"

size_t many2many::nnodes(size_t element) { return getsize(nodesfromelement.lnods[element]); }
size_t many2many::nelems(size_t node) { return getsize(elementsfromnode.lnods[node]); }
void setnumberofelements(many2many &rel, size_t nelem) { setnelem(rel.nodesfromelement, nelem); }
void setnodesforelement(many2many &rel, size_t element, seque<size_t> const &nodes)
{
    rel.nodesfromelement.lnods[element] = nodes;
}
size_t appendelement(many2many &rel, seque<size_t> const &nodes)
{
    size_t newel = hidden::appendelement(rel.nodesfromelement, nodes);

    return newel;
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
    setallpointers(relc);
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
    setallpointers(relc);
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
    setallpointers(relc);
}
void subtraction(const many2many &rela, bool transposea, const many2many &relb, bool transposeb, many2many &relc)
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
    subtraction(*a, *b, relc.nodesfromelement);
    setallpointers(relc);
}

void setallpointers(many2many &rel)
{
    transpose(rel.nodesfromelement, rel.elementsfromnode);

    setsize(rel.nodelocation, rel.elementsfromnode.nelem);
    for (size_t node = 0; node < rel.elementsfromnode.nelem; ++node)
    {
        setsize(rel.nodelocation[node], getsize(rel.elementsfromnode.lnods[node]));
    }
    hidden::lst nextlocalelementofnode(rel.elementsfromnode.nelem, 0);
    size_t nelem = getsize(rel.nodesfromelement.lnods);
    for (size_t element = 0; element < nelem; ++element)
    {
        const hidden::lst &nodes = rel.nodesfromelement.lnods[element];
        size_t nnode = getsize(nodes);
        for (size_t localnode = 0; localnode < nnode; ++localnode)
        {
            size_t node = nodes[localnode];
            rel.nodelocation[node][nextlocalelementofnode[node]++] = localnode;
        }
    }
}
hidden::lst getelementsfromnodes(many2many const &rel, hidden::lst const &nodes)
{
    seque<size_t> elems;
    if (getsize(nodes) == 0)
    {
        return elems;
    }
    elems = rel.elementsfromnode.lnods[nodes[0]];
    for (size_t elem = 1; elem < getsize(nodes); ++elem)
        elems = getintersection(elems, rel.elementsfromnode.lnods[nodes[elem]]);
    seque<size_t> ret;
    size_t counter{0};
    for (size_t elem = 0; elem < getsize(elems); ++elem)
    {
        if (getsize(rel.nodesfromelement.lnods[elems[elem]]) == getsize(nodes))
            append(ret, elems[elem]);
    }
    return ret;
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
void permutenodes(many2many &rel, hidden::lst const &newnodefromold)
{
    permutenodes(rel.nodesfromelement, newnodefromold);
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
seque<size_t> lexiorder(many2many const &rel) { return lexiorder(rel.nodesfromelement); }
seque<size_t> toporder(many2many const &rel) { return toporder(rel.nodesfromelement); }
size_t getlocalnodeposition(many2many const &rel, size_t node, size_t localelement)
{
    return rel.nodelocation[node][localelement];
}
