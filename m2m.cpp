#include "m2m.hpp"
#include "o2m.hpp"
#include <algorithm>
#include <cassert>

int appendelement(m2m &rel, crowview nodes)
{
    const int newel = appendelement(rel.nfrome, nodes);
    rel.isupdated = false;
    rel.islocated = false;
    return newel;
}

void setnumberofelements(m2m &rel, int nelem)
{
    setsize(rel.nfrome, nelem);
    rel.isupdated = false;
    rel.islocated = false;
    rel.efromn.maxnode = nelem - 1;
}

void synchronize(m2m &rel)
{
    if (!rel.isupdated)
    {
        rel.efromn = Tr(rel.nfrome);
        rel.isupdated = true;
        rel.islocated = false;
    }
}

void synchronizelocations(m2m &rel)
{
    synchronize(rel);
    if (!rel.islocated)
    {
        hidden::getlocations(rel.nfrome, rel.efromn, rel.nodeloc, rel.elementloc);
        rel.islocated = true;
    }
}

seque<int> getelementswithnodes(m2m &rel, crowview nodes)
{
    if (!rel.isupdated)
        synchronize(rel);
    seque<int> elems;
    const int n = nodes.n;
    if (n == 0)
        return elems;
    const int nn = rel.efromn.nelem;
    // the shortest list seeds the intersection
    int first = 0;
    int best = -1;
    for (int i = 0; i < n; ++i)
    {
        const int v = nodes.p[i];
        const int len = (v >= 0 && v < nn) ? rel.efromn.nnodes(v) : 0;
        if (best < 0 || len < best)
        {
            best = len;
            first = i;
        }
    }
    if (best <= 0)
        return elems;
    elems = rel.efromn[nodes.p[first]];
    for (int i = 0; i < n && elems.size > 0; ++i)
    {
        if (i == first)
            continue;
        crowview other = rel.efromn[nodes.p[i]];
        // in-place sorted merge (both lists ascending)
        int a = 0, b = 0, k = 0;
        while (a < elems.size && b < other.n)
        {
            if (elems[a] < other.p[b])
                ++a;
            else if (other.p[b] < elems[a])
                ++b;
            else
            {
                elems[k++] = elems[a];
                ++a;
                ++b;
            }
        }
        setsize(elems, k);
    }
    return elems;
}

seque<int> getelementsfromnodes(m2m &rel, crowview nodes)
{
    seque<int> elems = getelementswithnodes(rel, nodes), ret;
    for (int i = 0; i < elems.size; ++i)
        if (rel.nfrome.nnodes(elems[i]) == nodes.n)
            append(ret, elems[i]);
    return ret;
}

seque<int> getelementneighbors(m2m &rel, int element)
{
    if (!rel.isupdated)
        synchronize(rel);
    seque<int> neighbors;
    for (int node: rel.nfrome[element])
        for (int other: rel.efromn[node])
            if (other != element)
                append(neighbors, other);
    setorderedandunique(neighbors);
    return neighbors;
}

seque<int> getnodeneighbors(m2m &rel, int node)
{
    if (!rel.isupdated)
        synchronize(rel);
    seque<int> neighbors;
    for (int elem: rel.efromn[node])
        for (int other: rel.nfrome[elem])
            if (other != node)
                append(neighbors, other);
    setorderedandunique(neighbors);
    return neighbors;
}

void compresselements(m2m &rel, seque<int> const &oldelementfromnew)
{
    if (rel.nfrome.nelem > 0)
    {
        hidden::compresselements(rel.nfrome, oldelementfromnew);
        rel.isupdated = false;
        synchronize(rel);
    }
}

void permutenodes(m2m &rel, seque<int> const &newnodefromold)
{
    hidden::permutenodes(rel.nfrome, newnodefromold);
    rel.isupdated = false;
    synchronize(rel);
}

m2m getelementstoelements(m2m &rel)
{
    m2m elementstoelements;
    if (!rel.isupdated)
        synchronize(rel);
    elementstoelements.nfrome = rel.nfrome * rel.efromn;
    elementstoelements.isupdated = false;
    return elementstoelements;
}

m2m getnodestonodes(m2m &rel)
{
    m2m nodestonodes;
    if (!rel.isupdated)
        synchronize(rel);
    nodestonodes.nfrome = rel.efromn * rel.nfrome;
    nodestonodes.isupdated = false;
    return nodestonodes;
}

o2m getcliques(m2m &rel)
{
    if (!rel.isupdated)
        synchronize(rel);
    return getcliques(rel.nfrome, rel.efromn);
}

seque<int> getorder(m2m const &rel) { return getorder(rel.nfrome); }

seque<int> gettoporder(m2m const &rel) { return gettoporder(rel.nfrome); }
