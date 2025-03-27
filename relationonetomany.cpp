#include "relationonetomany.hpp"
#include <algorithm>
#include <cassert>
#include <queue>
#include <stdexcept>
#include <vector>
#ifdef _OPENMP
#include <omp.h>
#endif

// No parallel work needed here.
void setnelem(relationonetomany &rel, size_t nelem)
{
    rel.nelems = nelem;
    setsize(rel.lnods, nelem);
}

size_t appendelement(relationonetomany &rel, const hidden::lst &nodes)
{
    rel.nelems++;
    setsize(rel.lnods, rel.nelems);
    assert(getsize(rel.lnods) == rel.nelems);
    rel.lnods[rel.nelems - 1] = nodes;
    // Compute the maximum node in 'nodes' in parallel.
    {
        size_t local_max = rel.maxnodenumber;
#ifdef _OPENMP
#pragma omp parallel for reduction(max : local_max)
#endif
        for (size_t i = 0; i < getsize(nodes); ++i)
        {
            local_max = std::max(local_max, nodes[i]);
        }
        rel.maxnodenumber = local_max;
    }
    return rel.nelems - 1;
}

void transpose(const relationonetomany &rel, relationonetomany &relt)
{
    const size_t numNodes = rel.maxnodenumber + 1;
    std::vector<size_t> counts(numNodes, 0);
    // Parallelize counting across the rows using atomic increments.
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for (size_t i = 0; i < getsize(rel.lnods); ++i)
    {
        for (size_t node: rel.lnods[i])
        {
#ifdef _OPENMP
#pragma omp atomic
#endif
            counts[node]++;
        }
    }
    setnelem(relt, numNodes);
    // Set sizes for each node’s list in 'relt'
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for (size_t i = 0; i < numNodes; ++i)
    {
        setsize(relt.lnods[i], counts[i]);
    }
    // Here the original code used a shared marker and generation.
    // For thread safety we leave this loop sequential.
    std::vector<size_t> genMarker(numNodes, 0);
    size_t currentGeneration = 1;
    for (size_t e = 0; e < rel.nelems; ++e)
    {
        for (size_t node: rel.lnods[e])
        {
            if (genMarker[node] != currentGeneration)
            {
                counts[node] = 0;
                genMarker[node] = currentGeneration;
            }
            relt.lnods[node][counts[node]++] = e;
            relt.maxnodenumber = std::max(relt.maxnodenumber, e);
        }
    }
}

void times(const relationonetomany &rela, const relationonetomany &relb, relationonetomany &relc)
{
    // Preallocate result storage.
    setnelem(relc, rela.nelems);
    relc.maxnodenumber = relb.maxnodenumber;
    // Instead of using a shared marker and generation, allocate a local marker for each row.
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for (size_t r = 0; r < rela.nelems; ++r)
    {
        // Allocate a local marker for this row.
        std::vector<size_t> local_marker(relc.maxnodenumber + 1, 0);
        size_t count = 0;
        // First pass: count distinct bCols.
        for (size_t aCol: rela.lnods[r])
        {
            for (size_t bCol: relb.lnods[aCol])
            {
                if (local_marker[bCol] == 0)
                {
                    local_marker[bCol] = 1;
                    ++count;
                }
            }
        }
        setsize(relc.lnods[r], count);
        size_t idx = 0;
        // Second pass: write out the bCols.
        for (size_t aCol: rela.lnods[r])
        {
            for (size_t bCol: relb.lnods[aCol])
            {
                if (local_marker[bCol] == 1)
                {
                    local_marker[bCol] = 2; // mark as written
                    relc.lnods[r][idx++] = bCol;
                }
            }
        }
    }
}

void plusunion(const relationonetomany &rela, const relationonetomany &relb, relationonetomany &relc)
{
    size_t maxelem = std::max(rela.nelems, relb.nelems);
    setnelem(relc, maxelem);
    relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
    // Process each row independently by allocating a local marker.
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for (size_t row = 0; row < maxelem; ++row)
    {
        std::vector<size_t> local_marker(relc.maxnodenumber + 1, 0);
        size_t len = 0;
        if (row < rela.nelems)
        {
            for (size_t node: rela.lnods[row])
            {
                len++;
                local_marker[node] = len;
            }
        }
        if (row < relb.nelems)
        {
            for (size_t node: relb.lnods[row])
            {
                if (local_marker[node] == 0)
                {
                    len++;
                    local_marker[node] = len;
                }
            }
        }
        setsize(relc.lnods[row], len);
    }
    // Now fill in the union for each row.
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for (size_t row = 0; row < maxelem; ++row)
    {
        std::vector<size_t> local_marker(relc.maxnodenumber + 1, 0);
        size_t len = 0;
        if (row < rela.nelems)
        {
            for (size_t node: rela.lnods[row])
            {
                relc.lnods[row][len] = node;
                len++;
                local_marker[node] = len;
            }
        }
        if (row < relb.nelems)
        {
            for (size_t node: relb.lnods[row])
            {
                if (local_marker[node] == 0)
                {
                    relc.lnods[row][len] = node;
                    len++;
                    local_marker[node] = len;
                }
            }
        }
    }
}

void intersection(const relationonetomany &a, const relationonetomany &b, relationonetomany &c)
{
    const size_t nRows = std::min(a.nelems, b.nelems);
    setnelem(c, nRows);
    c.maxnodenumber = std::max(a.maxnodenumber, b.maxnodenumber);
    // Process each row independently using a local marker.
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for (size_t r = 0; r < nRows; ++r)
    {
        std::vector<size_t> local_marker(c.maxnodenumber + 1, 0);
        for (size_t node: b.lnods[r])
        {
            local_marker[node] = 1;
        }
        size_t count = 0;
        for (size_t node: a.lnods[r])
        {
            if (local_marker[node] == 1)
            {
                ++count;
            }
        }
        setsize(c.lnods[r], count);
        size_t idx = 0;
        for (size_t node: a.lnods[r])
        {
            if (local_marker[node] == 1)
            {
                c.lnods[r][idx++] = node;
            }
        }
    }
}

void difference(const relationonetomany &rela, const relationonetomany &relb, relationonetomany &relc)
{
    const size_t nRows = std::min(rela.nelems, relb.nelems);
    setnelem(relc, nRows);
    relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
    // Process each row independently.
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for (size_t r = 0; r < nRows; ++r)
    {
        std::vector<size_t> local_marker(relc.maxnodenumber + 1, 0);
        for (size_t node: relb.lnods[r])
        {
            local_marker[node] = 1;
        }
        size_t count = 0;
        for (size_t node: rela.lnods[r])
        {
            if (local_marker[node] != 1)
            {
                ++count;
            }
        }
        setsize(relc.lnods[r], count);
        size_t idx = 0;
        for (size_t node: rela.lnods[r])
        {
            if (local_marker[node] != 1)
            {
                relc.lnods[r][idx++] = node;
            }
        }
    }
}

void toporder(const relationonetomany &rel, hidden::lst &order)
{
    setsize(order, 0);
    std::vector<size_t> inDegree(rel.nelems, 0);
    // Compute in-degrees in parallel (using atomic increments)
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for (size_t i = 0; i < getsize(rel.lnods); ++i)
    {
        for (size_t node: rel.lnods[i])
        {
#ifdef _OPENMP
#pragma omp atomic
#endif
            ++inDegree[node];
        }
    }
    std::queue<size_t> q;
    for (size_t i = 0; i < inDegree.size(); ++i)
    {
        if (inDegree[i] == 0)
        {
            q.push(i);
        }
    }
    // The remainder of the topological sort is inherently sequential.
    while (!q.empty())
    {
        size_t cur = q.front();
        q.pop();
        append(order, cur);
        for (size_t nbr: rel.lnods[cur])
        {
            if (--inDegree[nbr] == 0)
            {
                q.push(nbr);
            }
        }
    }
    if (getsize(order) != getsize(rel.lnods))
    {
        throw std::runtime_error("The relation contains cycles, topological sort not possible.");
    }
}

void lexiorder(const relationonetomany &rel, hidden::lst &orderofelements) { orderofelements = getorder(rel.lnods); }

void indicesfromorder(const relationonetomany &rel, const hidden::lst &elemOrder, hidden::lst &oldFromNew,
                      hidden::lst &newFromOld)
{
    indicesfromorder(rel.lnods, elemOrder, oldFromNew, newFromOld);
}

void compresselements(relationonetomany &rel, const hidden::lst &oldelementfromnew)
{
    rel.lnods = rel.lnods(oldelementfromnew);
    rel.nelems = getsize(oldelementfromnew);
    // Compute the maximum node via a parallel reduction.
    size_t local_max = 0;
#ifdef _OPENMP
#pragma omp parallel for reduction(max : local_max)
#endif
    for (size_t i = 0; i < getsize(rel.lnods); ++i)
    {
        for (size_t j = 0; j < getsize(rel.lnods[i]); ++j)
        {
            local_max = std::max(local_max, rel.lnods[i][j]);
        }
    }
    rel.maxnodenumber = local_max;
}

void compressnodes(relationonetomany &rel, const hidden::lst &newnodefromold)
{
    // Process each row independently.
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for (size_t i = 0; i < getsize(rel.lnods); ++i)
    {
        rel.lnods[i] = newnodefromold(rel.lnods[i]);
    }
    // Then update maxnodenumber with a parallel reduction.
    size_t local_max = 0;
#ifdef _OPENMP
#pragma omp parallel for reduction(max : local_max)
#endif
    for (size_t i = 0; i < getsize(rel.lnods); ++i)
    {
        for (size_t j = 0; j < getsize(rel.lnods[i]); ++j)
        {
            local_max = std::max(local_max, rel.lnods[i][j]);
        }
    }
    rel.maxnodenumber = local_max;
}
