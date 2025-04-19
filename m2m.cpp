#include "m2m.hpp"
#include "o2m.hpp"
#include <cassert>

int appendelement(m2m &rel, seque<int> const &nodes)
{
    int newel = appendelement(rel.nfrome, nodes);
    rel.isupdated = false;
    return newel;
}

void setnumberofelements(m2m &rel, int nelem)
{
    setsize(rel.nfrome.lnods, nelem);
    rel.isupdated = false;
    rel.efromn.maxnode = nelem - 1;
}

void setnodesforelement(m2m &rel, int element, seque<int> const &nodes)
{
    setnodesforelement(rel.efromn, element, nodes);
    rel.isupdated = false;
}

void synchronize(m2m &rel)
{
    if (!rel.isupdated)
    {
        // Create inverse mapping from nodes to elements
        rel.efromn = Tr(rel.nfrome);
        rel.nodeloc = hidden::getnodepositions(rel.nfrome, rel.efromn);
        rel.elementloc = hidden::getelementpositions(rel.nfrome, rel.efromn);
        rel.isupdated = true;
    }
}

seque<int> getelementswithnodes(m2m const &rel, seque<int> const &nodes)
{
    if (!rel.isupdated)
    {
        synchronize(const_cast<m2m &>(rel));
    }
    seque<int> elems;
    if (getsize(nodes) == 0)
        return elems;
    elems = rel.efromn.lnods[nodes[0]];
    for (int i = 1; i < getsize(nodes); ++i)
        elems = getintersection(elems, rel.efromn.lnods[nodes[i]]);
    return elems;
}

seque<int> getelementsfromnodes(m2m const &rel, seque<int> const &nodes)
{
    assert(rel.isupdated);
    seque<int> elems = getelementswithnodes(rel, nodes), ret;
    for (int i = 0; i < getsize(elems); ++i)
        if (getsize(rel.nfrome.lnods[elems[i]]) == getsize(nodes))
            append(ret, elems[i]);
    return ret;
}

seque<int> getelementneighbours(m2m const &rel, int element)
{
    if (!rel.isupdated)
    {
        synchronize(const_cast<m2m &>(rel));
    }
    seque<int> neighbours;
    setsize(neighbours, 0);
    const seque<int> &elementNodes = rel.nfrome.lnods[element];
    for (int i = 0; i < getsize(elementNodes); ++i)
    {
        int node = elementNodes[i];
        const seque<int> &nodeElements = rel.efromn.lnods[node];
        for (int j = 0; j < getsize(nodeElements); ++j)
        {
            int other = nodeElements[j];
            if (other != element)
                append(neighbours, other);
        }
    }
    setorderedandunique(neighbours);
    return neighbours;
}

seque<int> getnodeneighbours(m2m const &rel, int node)
{
    assert(rel.isupdated);
    seque<int> neighbours;
    setsize(neighbours, 0);
    const seque<int> &elements = rel.efromn.lnods[node];
    for (int i = 0; i < getsize(elements); ++i)
    {
        int elem = elements[i];
        const seque<int> &nodes = rel.nfrome.lnods[elem];
        for (int j = 0; j < getsize(nodes); ++j)
        {
            int other = nodes[j];
            if (other != node)
                append(neighbours, other);
        }
    }
    setorderedandunique(neighbours);
    return neighbours;
}

void compresselements(m2m &rel, seque<int> const &oldelementfromnew)
{
    if (rel.nfrome.nelem > 0)
    {
        hidden::compresselements(rel.nfrome, oldelementfromnew);
        synchronize(rel);
    }
}

void permutenodes(m2m &rel, seque<int> const &newnodefromold)
{
    hidden::permutenodes(rel.nfrome, newnodefromold);
    synchronize(rel);
}

m2m getelementstoelements(m2m const &rel)
{
    m2m elementstoelements;
    assert(rel.isupdated);
    elementstoelements.nfrome = rel.nfrome * rel.efromn;
    elementstoelements.isupdated = false;
    return elementstoelements;
}

m2m getnodestonodes(m2m const &rel)
{
    m2m nodestonodes;
    assert(rel.isupdated);
    nodestonodes.nfrome = rel.efromn * rel.nfrome;
    nodestonodes.isupdated = false;
    return nodestonodes;
}

seque<seque<int> > getcliques(m2m const &rel)
{
    assert(rel.isupdated);
    return getcliques(rel.nfrome, rel.efromn);
}

seque<int> getorder(m2m const &rel) { return getorder(rel.nfrome); }

seque<int> gettoporder(m2m const &rel) { return gettoporder(rel.nfrome); }
