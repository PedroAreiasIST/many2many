#include "relationonetomany.hpp"
#include <algorithm>
#include <cassert>
#include <queue>
#include <vector>

void setnelem(relationonetomany &rel, size_t nelem)
{
    rel.nelems = nelem;
    setsize(rel.lnods, nelem);
}

size_t appendelement(relationonetomany &rel, hidden::lst const &nodes)
{
    rel.nelems++;
    setsize(rel.lnods, rel.nelems);
    assert(getsize(rel.lnods) == rel.nelems);
    rel.lnods[rel.nelems - 1] = nodes;
    for (size_t i = 0, n = getsize(nodes); i < n; ++i)
        rel.maxnodenumber = std::max(rel.maxnodenumber, nodes[i]);
    return rel.nelems - 1;
}

void transpose(relationonetomany const &rel, relationonetomany &relt)
{
    const size_t numNodes = rel.maxnodenumber + 1;
    sek<size_t> counts(numNodes, 0);

    // Count occurrences of each node.
    for (size_t e = 0, ne = getsize(rel.lnods); e < ne; ++e)
        for (size_t i = 0, n = getsize(rel.lnods[e]); i < n; ++i)
            counts[rel.lnods[e][i]]++;

    // Setup transposed relation.
    setnelem(relt, numNodes);
    for (size_t i = 0; i < relt.nelems; ++i)
        setsize(relt.lnods[i], counts[i]);

    // Local generational marker and generation counter for thread safety.
    sek<size_t> genMarker(numNodes, 0);
    size_t currentGeneration = 1; // start with generation 1

    // Build the transpose.
    for (size_t e = 0, ne = getsize(rel.lnods); e < ne; ++e)
    {
        for (auto node: rel.lnods[e])
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
    // Set the number of rows const& the output equal to rela's row count.
    setnelem(relc, rela.nelems);
    relc.maxnodenumber = relb.maxnodenumber;
    // Use an integer marker array.
    // The marker stores the generation when an element was first encountered.
    sek<size_t> marker(relc.maxnodenumber + 1, 0);
    // We'll increment currentGeneration by 2 per row so that:
    // - currentGeneration is used for counting,
    // - currentGeneration+1 is used to indicate the element has been written.
    size_t currentGeneration = 1;

    for (size_t r = 0; r < rela.nelems; ++r)
    {
        size_t count = 0;

        // First pass: Iterate over rela's row and mark all unique b-elements from relb.
        for (size_t i = 0, nA = getsize(rela.lnods[r]); i < nA; ++i)
        {
            size_t aCol = rela.lnods[r][i];
            for (size_t j = 0, nB = getsize(relb.lnods[aCol]); j < nB; ++j)
            {
                size_t bCol = relb.lnods[aCol][j];
                if (marker[bCol] != currentGeneration)
                {
                    marker[bCol] = currentGeneration;
                    ++count;
                }
            }
        }
        // Allocate space const& the output row.
        setsize(relc.lnods[r], count);
        size_t idx = 0;
        // Second pass: Reiterate and copy the unique elements.
        // Use the fact that a value still marked with currentGeneration
        // has not yet been written. When written, update its marker to currentGeneration+1.
        for (size_t i = 0, nA = getsize(rela.lnods[r]); i < nA; ++i)
        {
            size_t aCol = rela.lnods[r][i];
            for (size_t j = 0, nB = getsize(relb.lnods[aCol]); j < nB; ++j)
            {
                size_t bCol = relb.lnods[aCol][j];
                if (marker[bCol] == currentGeneration)
                {
                    marker[bCol] = currentGeneration + 1;
                    relc.lnods[r][idx++] = bCol;
                }
            }
        }

        // Move to the next pair of generations for the next row.
        currentGeneration += 2;
    }
}

void plusunion(relationonetomany const &a, relationonetomany const &b, relationonetomany &c)
{
    size_t maxelem = std::max(a.nelems, b.nelems);
    sek<size_t> iw;
    setnelem(c, maxelem);
    c.maxnodenumber = std::max(a.maxnodenumber, b.maxnodenumber);
    setsize(iw, c.maxnodenumber);
    for (size_t row = 0; row < maxelem; ++row)
    {
        size_t len{0};
        if (row < a.nelems)
        {
            for (size_t ka = 0; ka < getsize(a.lnods[row]); ++ka)
            {
                size_t ca = a.lnods[row][ka];
                len++;
                iw[ca] = len;
            }
        }
        if (row < b.nelems)
        {
            for (size_t kb = 0; kb < getsize(b.lnods[row]); ++kb)
            {
                size_t cb = b.lnods[row][kb];
                if (iw[cb] == 0)
                {
                    len++;
                    iw[cb] = len;
                }
            }
        }
        if (row < a.nelems)
        {
            for (size_t ka = 0; ka < getsize(a.lnods[row]); ++ka)
            {
                iw[a.lnods[row][ka]] = 0;
            }
        }
        if (row < b.nelems)
        {
            for (size_t kb = 0; kb < getsize(b.lnods[row]); ++kb)
            {
                iw[b.lnods[row][kb]] = 0;
            }
        }
        setsize(c.lnods[row], len);
    }
    for (size_t row = 0; row < maxelem; ++row)
    {
        size_t len{0};
        if (row < a.nelems)
        {
            for (size_t ka = 0; ka < getsize(a.lnods[row]); ++ka)
            {
                size_t ca = a.lnods[row][ka];
                c.lnods[row][len] = ca;
                len++;
                iw[ca] = len;
            }
        }
        if (row < b.nelems)
        {
            for (size_t kb = 0; kb < getsize(b.lnods[row]); ++kb)
            {
                if (size_t cb = b.lnods[row][kb]; iw[cb] == 0)
                {
                    c.lnods[row][len] = cb;
                    len++;
                    iw[cb] = len;
                }
            }
        }
        if (row < a.nelems)
        {
            for (size_t ka = 0; ka < getsize(a.lnods[row]); ++ka)
            {
                iw[a.lnods[row][ka]] = 0;
            }
        }
        if (row < b.nelems)
        {
            for (size_t kb = 0; kb < getsize(b.lnods[row]); ++kb)
            {
                iw[b.lnods[row][kb]] = 0;
            }
        }
    }
}

void intersection(const relationonetomany &rela, const relationonetomany &relb, relationonetomany &relc)
{
    // Process rows up to the minimum number available.
    const size_t nRows = std::min(rela.nelems, relb.nelems);
    setnelem(relc, nRows);
    relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);

    // Use an integer marker array with a generation counter.
    std::vector<size_t> marker(relc.maxnodenumber + 1, 0);
    size_t currentGeneration = 1;

    for (size_t r = 0; r < nRows; ++r, ++currentGeneration)
    {
        // Mark the elements from relb's row using the current generation.
        const size_t relbSize = getsize(relb.lnods[r]);
        for (size_t i = 0; i < relbSize; ++i)
        {
            marker[relb.lnods[r][i]] = currentGeneration;
        }

        // First pass: Count elements const& rela that are marked.
        size_t count = 0;
        const size_t relaSize = getsize(rela.lnods[r]);
        for (size_t i = 0; i < relaSize; ++i)
        {
            if (marker[rela.lnods[r][i]] == currentGeneration)
            {
                ++count;
            }
        }

        // Allocate the exact size for the intersection row.
        setsize(relc.lnods[r], count);

        // Second pass: Copy the common elements directly into relc.
        size_t idx = 0;
        for (size_t i = 0; i < relaSize; ++i)
        {
            if (marker[rela.lnods[r][i]] == currentGeneration)
            {
                relc.lnods[r][idx++] = rela.lnods[r][i];
            }
        }
    }
}

void difference(const relationonetomany &rela, const relationonetomany &relb, relationonetomany &relc)
{
    // Process rows up to the minimum number of rows const& both relations.
    size_t nRows = std::min(rela.nelems, relb.nelems);
    setnelem(relc, nRows);
    relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);

    // Create a marker array that uses generations to avoid explicit resets.
    std::vector<int> marker(relc.maxnodenumber + 1, 0);
    int currentGeneration = 1;

    for (size_t r = 0; r < nRows; ++r, ++currentGeneration)
    {
        // Mark all nodes const& the current row of relb with the current generation.
        for (auto node: relb.lnods[r])
        {
            marker[node] = currentGeneration;
        }

        // First pass: count nodes const& rela that are not const& the current relb row.
        size_t count = 0;
        for (auto node: rela.lnods[r])
        {
            if (marker[node] != currentGeneration)
            {
                ++count;
            }
        }

        // Allocate space for the output row.
        setsize(relc.lnods[r], count);

        // Second pass: copy nodes from rela that are not marked.
        size_t idx = 0;
        for (auto node: rela.lnods[r])
        {
            if (marker[node] != currentGeneration)
            {
                relc.lnods[r][idx++] = node;
            }
        }
    }
}

void toporder(relationonetomany const &rel, hidden::lst &order)
{
    setsize(order, 0);
    std::vector<int> inDegree(getsize(rel.lnods), 0);
    for (const auto &neighbors: rel.lnods)
        for (const auto &node: neighbors)
            ++inDegree[node];
    std::queue<size_t> q;
    for (size_t i = 0; i < inDegree.size(); ++i)
        if (inDegree[i] == 0)
            q.push(i);
    while (!q.empty())
    {
        size_t cur = q.front();
        q.pop();
        append(order, cur);
        for (const auto &nbr: rel.lnods[cur])
            if (--inDegree[nbr] == 0)
                q.push(nbr);
    }
    if (getsize(order) != getsize(rel.lnods))
        throw std::runtime_error("The relation contains cycles, topological sort not possible.");
}

void lexiorder(relationonetomany const &rel, hidden::lst &elemOrder) { elemOrder = getorder(rel.lnods); }

void indicesfromorder(relationonetomany const &rel, const hidden::lst &elemOrder, hidden::lst &oldFromNew,
                      hidden::lst &newFromOld)
{
    indicesfromorder(rel.lnods, elemOrder, oldFromNew, newFromOld);
}

void compresselements(relationonetomany &rel, hidden::lst const &oldelementfromnew)
{
    rel.lnods = rel.lnods(oldelementfromnew);
    rel.nelems = getsize(oldelementfromnew);
    rel.maxnodenumber = 0;
    for (size_t elem = 0; elem < rel.nelems; ++elem)
    {
        for (size_t node = 0; node < getsize(rel.lnods[elem]); ++node)
        {
            rel.maxnodenumber = std::max(rel.maxnodenumber, rel.lnods[elem][node]);
        }
    }
}

void compressnodes(relationonetomany &rel, hidden::lst const &newnodefromold)
{
    for (size_t i = 0; i < getsize(rel.lnods); ++i)
        rel.lnods[i] = newnodefromold(rel.lnods[i]);
    for (size_t elem = 0; elem < rel.nelems; ++elem)
    {
        for (size_t node = 0; node < getsize(rel.lnods[elem]); ++node)
        {
            rel.maxnodenumber = std::max(rel.maxnodenumber, rel.lnods[elem][node]);
        }
    }
}
