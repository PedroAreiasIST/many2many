#include "m2m.hpp"
#include "o2m.hpp"
#include <cassert>
#ifdef _OPENMP
#include <omp.h>
#endif

int m2m::nnodes() const
{
    return n2e.size();
}

int m2m::nelems() const
{
    return e2n.size();
}

int m2m::nnodes(int element) const
{
    return getsize(e2n[element]);
}

int m2m::nelems(int node) const
{
    return getsize(n2e[node]);
}

void setsize(m2m &rel, int nelem)
{
    setsize(rel.e2n, nelem);
}

void setnodesforelement(m2m &rel, int element, seque<int> const &nodes)
{
    setnodesforelement(rel.e2n, element, nodes);
    rel.isupdated = false;
}

int appendelement(m2m &rel, seque<int> const &nodes)
{
    int newel = appendelement(rel.e2n, nodes);
    rel.isupdated = false;
    return newel;
}

void setallpointers(m2m &rel)
{
    if (!rel.isupdated)
    {
        // Create inverse mapping from nodes to elements
        rel.n2e = Tr(rel.e2n);
        rel.nodelocation = hidden::getnodepositions(rel.e2n, rel.n2e);
        rel.elementlocation = hidden::getelementpositions(rel.e2n, rel.n2e);
        rel.isupdated = true;
    }
}

seque<int> getelementswithnodes(m2m const &rel, seque<int> const &nodes)
{
    assert(rel.isupdated);
    seque<int> elems;
    if (getsize(nodes) == 0)
        return elems;
    elems = rel.n2e.lnods[nodes[0]];
    for (int i = 1; i < getsize(nodes); ++i)
        elems = getintersection(elems, rel.n2e.lnods[nodes[i]]);
    return elems;
}

seque<int> getelementsfromnodes(m2m const &rel, seque<int> const &nodes)
{
    assert(rel.isupdated);
    seque<int> elems = getelementswithnodes(rel, nodes), ret;
    for (int i = 0; i < getsize(elems); ++i)
        if (getsize(rel.e2n.lnods[elems[i]]) == getsize(nodes))
            append(ret, elems[i]);
    return ret;
}

seque<int> getelementneighbours(m2m const &rel, int element)
{
    assert(rel.isupdated);
    seque<int> neighbours;
    setsize(neighbours, 0);
    const seque<int> &elementNodes = rel.e2n.lnods[element];
    for (int i = 0; i < getsize(elementNodes); ++i)
    {
        int node = elementNodes[i];
        const seque<int> &nodeElements = rel.n2e.lnods[node];
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
    const seque<int> &elements = rel.n2e.lnods[node];
    for (int i = 0; i < getsize(elements); ++i)
    {
        int elem = elements[i];
        const seque<int> &nodes = rel.e2n.lnods[elem];
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

void indicesfromorder(m2m const &rel, const seque<int> &elementorder,
                      seque<int> &oldfromnew, seque<int> &newfromold)
{
    hidden::indicesfromorder(rel.e2n, elementorder, oldfromnew, newfromold);
}

void compresselements(m2m &rel, seque<int> const &oldelementfromnew)
{
    hidden::compresselements(rel.e2n, oldelementfromnew);
    setallpointers(rel);
}

void permutenodes(m2m &rel, seque<int> const &newnodefromold)
{
    hidden::permutenodes(rel.e2n, newnodefromold);
    setallpointers(rel);
}

void getelementstoelements(m2m const &rel, m2m &elementstoelements)
{
    assert(rel.isupdated);
    elementstoelements.e2n = rel.e2n * rel.n2e;
    elementstoelements.isupdated = false;
}

void getnodestonodes(m2m const &rel, m2m &nodestonodes)
{
    assert(rel.isupdated);
    nodestonodes.e2n = rel.n2e * rel.e2n;
    nodestonodes.isupdated = false;
}

seque<int> lexiorder(m2m const &rel) { return lexiorder(rel.e2n); }

seque<int> toporder(m2m const &rel) { return toporder(rel.e2n); }
