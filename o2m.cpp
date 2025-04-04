#include "o2m.hpp"
#include <algorithm>
#include <cassert>
#include <queue>
#include <stdexcept>
#include <vector>
#ifdef _OPENMP
#include <omp.h>
#endif

namespace hidden
{
    // No parallel work needed here.
    void setnelem(hidden::o2m &rel, size_t nelem)
    {
        rel.nelem = nelem;
        setsize(rel.lnods, nelem);
    }

    size_t appendelement(hidden::o2m &rel, seque<size_t> const &nodes)
    {
        rel.nelem++;
        setsize(rel.lnods, rel.nelem);
        assert(getsize(rel.lnods) == rel.nelem);
        rel.lnods[rel.nelem - 1] = nodes;
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
        return rel.nelem - 1;
    }

    void transpose(const o2m &rel, o2m &relt)
    {
        const size_t numberofnodes = rel.maxnodenumber + 1;
        std::vector<size_t> counts(numberofnodes, 0);
        // Parallelize counting across the rows using atomic increments.
#ifdef _OPENMP
#pragma omp parallel for
#endif
        for (size_t localelement = 0; localelement < getsize(rel.lnods); ++localelement)
        {
            for (size_t node: rel.lnods[localelement])
            {
#ifdef _OPENMP
#pragma omp atomic
#endif
                counts[node]++;
            }
        }
        setnelem(relt, numberofnodes);
        // Set sizes for each node’s list in 'relt'
#ifdef _OPENMP
#pragma omp parallel for
#endif
        for (size_t localnode = 0; localnode < numberofnodes; ++localnode)
        {
            setsize(relt.lnods[localnode], counts[localnode]);
        }
        // Here the original code used a shared marker and generation.
        // For thread safety we leave this loop sequential.
        std::vector<size_t> genmarker(numberofnodes, 0);
        size_t currentgeneration = 1;
        for (size_t element = 0; element < rel.nelem; ++element)
        {
            for (size_t node: rel.lnods[element])
            {
                if (genmarker[node] != currentgeneration)
                {
                    counts[node] = 0;
                    genmarker[node] = currentgeneration;
                }
                relt.lnods[node][counts[node]++] = element;
                relt.maxnodenumber = std::max(relt.maxnodenumber, element);
            }
        }
    }

    void multiplication(const o2m &rela, const o2m &relb, o2m &relc)
    {
        // Preallocate result storage.
        setnelem(relc, rela.nelem);
        relc.maxnodenumber = relb.maxnodenumber;
        // Instead of using a shared marker and generation, allocate a local marker for each row.
#ifdef _OPENMP
#pragma omp parallel for
#endif
        for (size_t aRow = 0; aRow < rela.nelem; ++aRow)
        {
            // Allocate a local marker for this row.
            std::vector<size_t> local_marker(relc.maxnodenumber + 1, 0);
            size_t count = 0;
            // First pass: count distinct bCols.
            for (size_t aCol: rela.lnods[aRow])
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
            setsize(relc.lnods[aRow], count);
            size_t idx = 0;
            // Second pass: write out the bCols.
            for (size_t aCol: rela.lnods[aRow])
            {
                for (size_t bCol: relb.lnods[aCol])
                {
                    if (local_marker[bCol] == 1)
                    {
                        local_marker[bCol] = 2; // mark as written
                        relc.lnods[aRow][idx++] = bCol;
                    }
                }
            }
        }
    }

    void addition(const o2m &rela, const o2m &relb, o2m &relc)
    {
        size_t maxelem = std::max(rela.nelem, relb.nelem);
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
            if (row < rela.nelem)
            {
                for (size_t node: rela.lnods[row])
                {
                    len++;
                    local_marker[node] = len;
                }
            }
            if (row < relb.nelem)
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
            if (row < rela.nelem)
            {
                for (size_t node: rela.lnods[row])
                {
                    relc.lnods[row][len] = node;
                    len++;
                    local_marker[node] = len;
                }
            }
            if (row < relb.nelem)
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

    void intersection(const o2m &a, const o2m &b, o2m &c)
    {
        const size_t nRows = std::min(a.nelem, b.nelem);
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

    void subtraction(const o2m &rela, const o2m &relb, o2m &relc)
    {
        const size_t nRows = std::min(rela.nelem, relb.nelem);
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

    seque<size_t> toporder(const o2m &rel)
    {
        seque<size_t> order;
        setsize(order, 0);
        std::vector<size_t> inDegree(rel.nelem, 0);
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
        return order;
    }

    seque<size_t> lexiorder(const o2m &rel) { return getorder(rel.lnods); }

    void indicesfromorder(const o2m &rel, const seque<size_t> &elemOrder, seque<size_t> &oldFromNew,
                          seque<size_t> &newFromOld)
    {
        indicesfromorder(rel.lnods, elemOrder, oldFromNew, newFromOld);
    }

    void compresselements(o2m &rel, const seque<size_t> &oldelementfromnew)
    {
        rel.lnods = rel.lnods(oldelementfromnew);
        rel.nelem = getsize(oldelementfromnew);
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

    void permutenodes(o2m &rel, const seque<size_t> &newnodefromold)
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
} // namespace hidden
