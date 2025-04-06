#include "m2m.hpp"

size_t m2m::nnodes(size_t element) { return getsize(nodesfromelement.lnods[element]); }
size_t m2m::nelems(size_t node) { return getsize(elementsfromnode.lnods[node]); }
void setnumberofelements(m2m &rel, size_t nelem)
{ setnumberofelements(rel.nodesfromelement, nelem); }
void setnodesforelement(m2m &rel, size_t element, seque<size_t> const &nodes)
{
    setnodesforelement(rel.nodesfromelement, element, nodes);
    rel.isupdated=false;
}
size_t appendelement(m2m &rel, seque<size_t> const &nodes)
{
    size_t newel = hidden::appendelement(rel.nodesfromelement, nodes);
    rel.isupdated=false;
    return newel;
}
void multiplication(const m2m &rela, bool transposea, const m2m &relb, bool transposeb, m2m &relc)
{
    hidden::o2m const *a;
    hidden::o2m const *b;
    if (transposea)
    {
        assert(rela.isupdated);
        a = &rela.elementsfromnode;
    } else
    {
        a = &rela.nodesfromelement;
    }
    if (transposeb)
    {
        assert(relb.isupdated);
        b = &relb.elementsfromnode;
    } else
    {
        b = &relb.nodesfromelement;
    }
    multiplication(*a, *b, relc.nodesfromelement);
   // setallpointers(relc);
    relc.isupdated=false;
}
void addition(const m2m &rela, bool transposea, const m2m &relb, bool transposeb, m2m &relc)
{
    hidden::o2m const *a;
    hidden::o2m const *b;
    if (transposea)
    {
        assert(rela.isupdated);
        a = &rela.elementsfromnode;
    } else
    {
        a = &rela.nodesfromelement;
    }
    if (transposeb)
    {
        assert(relb.isupdated);
        b = &relb.elementsfromnode;
    } else
    {
        b = &relb.nodesfromelement;
    }
    addition(*a, *b, relc.nodesfromelement);
    //setallpointers(relc);
    relc.isupdated=false;
}
void intersection(const m2m &rela, bool transposea, const m2m &relb, bool transposeb, m2m &relc)
{
    hidden::o2m const *a;
    hidden::o2m const *b;
    if (transposea)
    {
        assert(rela.isupdated);
        a = &rela.elementsfromnode;
    } else
    {
        a = &rela.nodesfromelement;
    }
    if (transposeb)
    {
        assert(relb.isupdated);
        b = &relb.elementsfromnode;
    } else
    {
        b = &relb.nodesfromelement;
    }
    intersection(*a, *b, relc.nodesfromelement);
    //setallpointers(relc);
    relc.isupdated=false;
}
void subtraction(const m2m &rela, bool transposea, const m2m &relb, bool transposeb, m2m &relc)
{
    hidden::o2m const *a;
    hidden::o2m const *b;
    if (transposea)
    {
        assert(rela.isupdated);
        a = &rela.elementsfromnode;
    } else
    {
        a = &rela.nodesfromelement;
    }
    if (transposeb)
    {
        assert(relb.isupdated);
        b = &relb.elementsfromnode;
    } else
    {
        b = &relb.nodesfromelement;
    }
    subtraction(*a, *b, relc.nodesfromelement);
    //setallpointers(relc);
    relc.isupdated=false;
}

void setallpointers(m2m &rel)
{
    if (!rel.isupdated) {
        transpose(rel.nodesfromelement, rel.elementsfromnode);
        setsize(rel.nodelocation, rel.elementsfromnode.nelem);
        for (size_t node = 0; node < rel.elementsfromnode.nelem; ++node)
        {
            setsize(rel.nodelocation[node], getsize(rel.elementsfromnode.lnods[node]));
        }
        seque<size_t> nextlocalelementofnode(rel.elementsfromnode.nelem, 0);
        size_t nelem = getsize(rel.nodesfromelement.lnods);
        for (size_t element = 0; element < nelem; ++element)
        {
            const seque<size_t> &nodes = rel.nodesfromelement.lnods[element];
            size_t nnode = getsize(nodes);
            for (size_t localnode = 0; localnode < nnode; ++localnode)
            {
                size_t node = nodes[localnode];
                rel.nodelocation[node][nextlocalelementofnode[node]++] = localnode;
            }
        }
        rel.isupdated=true;
    }
}

seque<size_t> getelementswithnodes(m2m const &rel, seque<size_t> const &nodes)
{
    assert(rel.isupdated);
    seque<size_t> elems;
    if (getsize(nodes) == 0)
    {
        return elems;
    }
    elems = rel.elementsfromnode.lnods[nodes[0]];
    for (size_t elem = 1; elem < getsize(nodes); ++elem)
        elems = getintersection(elems, rel.elementsfromnode.lnods[nodes[elem]]);
    return elems;
}
seque<size_t> getelementsfromnodes(m2m const &rel, seque<size_t> const &nodes)
{
    assert(rel.isupdated);
    seque<size_t> elems = getelementswithnodes(rel, nodes);
    seque<size_t> ret;
    for (size_t elem = 0; elem < getsize(elems); ++elem)
    {
        if (getsize(rel.nodesfromelement.lnods[elems[elem]]) == getsize(nodes))
            append(ret, elems[elem]);
    }
    return ret;
}
seque<size_t> getelementneighbours(m2m const &rel, size_t element)
{
    assert(rel.isupdated);
    seque<size_t> neighbours;
    setsize(neighbours, 0);
    const seque<size_t> &elementNodes = rel.nodesfromelement.lnods[element];
    for (size_t nodePos = 0, nodeCount = getsize(elementNodes); nodePos < nodeCount; ++nodePos)
    {
        size_t node = elementNodes[nodePos];
        const seque<size_t> &nodeElements = rel.elementsfromnode.lnods[node];
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

seque<size_t> getnodeneighbours(m2m const &rel, size_t node){
    assert(rel.isupdated);
    seque<size_t> neighbours;
    setsize(neighbours, 0);
    const seque<size_t> &elementsofnode = rel.elementsfromnode.lnods[node];
    for (size_t nodePos = 0, nodeCount = getsize(elementsofnode); nodePos < nodeCount; ++nodePos)
    {
        size_t element = elementsofnode[nodePos];
        const seque<size_t> &elementnodes = rel.nodesfromelement.lnods[element];
        for (size_t elemPos = 0, elemCount = getsize(elementnodes); elemPos < elemCount; ++elemPos)
        {
            size_t othernode = elementnodes[elemPos];
            if (othernode != node)
            {
                append(neighbours, othernode);
            }
        }
    }
    setordered(neighbours);
    setunique(neighbours);
    return neighbours;
}

void indicesfromorder(m2m const &rel, const seque<size_t> &elementorder, seque<size_t> &oldfromnew,
                      seque<size_t> &newfromold)
{
    indicesfromorder(rel.nodesfromelement, elementorder, oldfromnew, newfromold);
}
void compresselements(m2m &rel, seque<size_t> const &oldelementfromnew)
{
    compresselements(rel.nodesfromelement, oldelementfromnew);
    setallpointers(rel);
}
void permutenodes(m2m &rel, seque<size_t> const &newnodefromold)
{
    permutenodes(rel.nodesfromelement, newnodefromold);
    setallpointers(rel);
}
void getelementstoelements(m2m const &rel, m2m &elementstoelements)
{
    assert(rel.isupdated);
    multiplication(rel.nodesfromelement, rel.elementsfromnode, elementstoelements.nodesfromelement);
  //  setallpointers(elementstoelements);
    elementstoelements.isupdated=false;
}
void getnodestonodes(m2m const &rel, m2m &nodestonodes)
{
    assert(rel.isupdated);
    multiplication(rel.elementsfromnode, rel.nodesfromelement, nodestonodes.nodesfromelement);
   // setallpointers(nodestonodes);
    nodestonodes.isupdated=false;
}
seque<size_t> lexiorder(m2m const &rel) { return lexiorder(rel.nodesfromelement); }
seque<size_t> toporder(m2m const &rel) { return toporder(rel.nodesfromelement); }
size_t getlocalnodeposition(m2m const &rel, size_t node, size_t localelement)
{
    assert(rel.isupdated);
    return rel.nodelocation[node][localelement];
}
