#include "m2m.hpp"
#include "o2m.hpp"
#include <cassert>
#ifdef _OPENMP
#include <omp.h>
#endif

namespace
{
    inline const o2m *select_o2m(const m2m &rel, bool transpose)
    {
        return transpose ? &rel.elementsfromnode : &rel.nodesfromelement;
    }

    inline o2m *select_o2m(m2m &rel, bool transpose)
    {
        return transpose ? &rel.elementsfromnode : &rel.nodesfromelement;
    }
} // namespace

int m2m::nnodes(int element)
{
    return getsize(nodesfromelement.lnods[element]);
}

int m2m::nelems(int node) { return getsize(elementsfromnode.lnods[node]); }

void setsize(m2m &rel, int nelem)
{
    setsize(rel.nodesfromelement, nelem);
}

void setnodesforelement(m2m &rel, int element, seque<int> const &nodes)
{
    setnodesforelement(rel.nodesfromelement, element, nodes);
    rel.isupdated = false;
}

int appendelement(m2m &rel, seque<int> const &nodes)
{
    int newel = appendelement(rel.nodesfromelement, nodes);
    rel.isupdated = false;
    return newel;
}

void multiplication(const m2m &rela, bool transposea, const m2m &relb,
                    bool transposeb, m2m &relc)
{
    const o2m *a = select_o2m(rela, transposea);
    const o2m *b = select_o2m(relb, transposeb);
    relc.nodesfromelement = (*a) * (*b);
    relc.isupdated = false;
}

void addition(const m2m &rela, bool transposea, const m2m &relb,
              bool transposeb, m2m &relc)
{
    const o2m *a = select_o2m(rela, transposea);
    const o2m *b = select_o2m(relb, transposeb);
    relc.nodesfromelement = (*a) + (*b);
    relc.isupdated = false;
}

void intersection(const m2m &rela, bool transposea, const m2m &relb,
                  bool transposeb, m2m &relc)
{
    const o2m *a = select_o2m(rela, transposea);
    const o2m *b = select_o2m(relb, transposeb);
    relc.nodesfromelement = ((*a) && (*b));
    relc.isupdated = false;
}

void subtraction(const m2m &rela, bool transposea, const m2m &relb,
                 bool transposeb, m2m &relc)
{
    const o2m *a = select_o2m(rela, transposea);
    const o2m *b = select_o2m(relb, transposeb);
    relc.nodesfromelement = (*a) - (*b);
    relc.isupdated = false;
}

void setallpointers(m2m &rel)
{
    if (!rel.isupdated)
    {
        // Create inverse mapping from nodes to elements
        rel.elementsfromnode = Tr(rel.nodesfromelement);
        // Prepare nodelocation storage - this stores position of each node within
        // elements
        setsize(rel.nodelocation, rel.elementsfromnode.nelem);
        for (int node = 0; node < rel.elementsfromnode.nelem; ++node)
        {
            setsize(rel.nodelocation[node],
                    getsize(rel.elementsfromnode.lnods[node]));
        }

        // Track the next position to fill for each node
        seque<int> nodePositionCounter(rel.elementsfromnode.nelem, 0);

        // Build the node location lookup table
        for (int element = 0; element < rel.nodesfromelement.nelem; ++element)
        {
            const auto &nodes = rel.nodesfromelement.lnods[element];
            for (int localPosition = 0; localPosition < getsize(nodes);
                 ++localPosition)
            {
                int node = nodes[localPosition];
                rel.nodelocation[node][nodePositionCounter[node]++] = localPosition;
            }
        }

        rel.isupdated = true;
    }
}

seque<int> getelementswithnodes(m2m const &rel, seque<int> const &nodes)
{
    assert(rel.isupdated);
    seque<int> elems;
    if (getsize(nodes) == 0)
        return elems;
    elems = rel.elementsfromnode.lnods[nodes[0]];
    for (int i = 1; i < getsize(nodes); ++i)
        elems = getintersection(elems, rel.elementsfromnode.lnods[nodes[i]]);
    return elems;
}

seque<int> getelementsfromnodes(m2m const &rel, seque<int> const &nodes)
{
    assert(rel.isupdated);
    seque<int> elems = getelementswithnodes(rel, nodes), ret;
    for (int i = 0; i < getsize(elems); ++i)
        if (getsize(rel.nodesfromelement.lnods[elems[i]]) == getsize(nodes))
            append(ret, elems[i]);
    return ret;
}

seque<int> getelementneighbours(m2m const &rel, int element)
{
    assert(rel.isupdated);
    seque<int> neighbours;
    setsize(neighbours, 0);
    const seque<int> &elementNodes = rel.nodesfromelement.lnods[element];
    for (int i = 0; i < getsize(elementNodes); ++i)
    {
        int node = elementNodes[i];
        const seque<int> &nodeElements = rel.elementsfromnode.lnods[node];
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
    const seque<int> &elements = rel.elementsfromnode.lnods[node];
    for (int i = 0; i < getsize(elements); ++i)
    {
        int elem = elements[i];
        const seque<int> &nodes = rel.nodesfromelement.lnods[elem];
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
    indicesfromorder(rel.nodesfromelement, elementorder, oldfromnew, newfromold);
}

void compresselements(m2m &rel, seque<int> const &oldelementfromnew)
{
    compresselements(rel.nodesfromelement, oldelementfromnew);
    setallpointers(rel);
}

void permutenodes(m2m &rel, seque<int> const &newnodefromold)
{
    permutenodes(rel.nodesfromelement, newnodefromold);
    setallpointers(rel);
}

void getelementstoelements(m2m const &rel, m2m &elementstoelements)
{
    assert(rel.isupdated);
    elementstoelements.nodesfromelement = rel.nodesfromelement * rel.elementsfromnode;
    elementstoelements.isupdated = false;
}

void getnodestonodes(m2m const &rel, m2m &nodestonodes)
{
    assert(rel.isupdated);
    nodestonodes.nodesfromelement = rel.elementsfromnode * rel.nodesfromelement;
    nodestonodes.isupdated = false;
}

seque<int> lexiorder(m2m const &rel) { return lexiorder(rel.nodesfromelement); }

seque<int> toporder(m2m const &rel) { return toporder(rel.nodesfromelement); }

int getlocalnodeposition(m2m const &rel, int node, int localelement)
{
    assert(rel.isupdated);
    return rel.nodelocation[node][localelement];
}
