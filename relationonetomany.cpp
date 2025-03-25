#include "relationonetomany.hpp"
#include <algorithm>
#include <cassert>
#include <queue>
#include <stack>
#include <vector>

void setnelem(relationonetomany &rel, size_t nelem)
{
    rel.nelem = nelem;
    setsize(rel.onetomany, nelem);
}

size_t appendelement(relationonetomany &rel, lst const &nodes)
{
    rel.nelem++;
    setsize(rel.onetomany, rel.nelem);
    assert(getsize(rel.onetomany) == rel.nelem);
    rel.onetomany[rel.nelem - 1] = nodes;
    for (size_t i = 0, n = getsize(nodes); i < n; ++i)
        rel.maxnodenumber = std::max(rel.maxnodenumber, nodes[i]);
    return rel.nelem - 1;
}

void transpose(relationonetomany const &rel, relationonetomany &relt)
{
    const size_t numNodes = rel.maxnodenumber + 1;
    sek<size_t> counts(numNodes, 0);

    // Count occurrences of each node.
    for (size_t e = 0, ne = getsize(rel.onetomany); e < ne; ++e)
        for (size_t i = 0, n = getsize(rel.onetomany[e]); i < n; ++i)
            counts[rel.onetomany[e][i]]++;

    // Setup transposed relation.
    setnelem(relt, numNodes);
    for (size_t i = 0; i < relt.nelem; ++i)
        setsize(relt.onetomany[i], counts[i]);

    // Local generational marker and generation counter for thread safety.
    sek<size_t> genMarker(numNodes, 0);
    size_t currentGeneration = 1; // start with generation 1

    // Build the transpose.
    for (size_t e = 0, ne = getsize(rel.onetomany); e < ne; ++e)
    {
        for (auto node: rel.onetomany[e])
        {
            if (genMarker[node] != currentGeneration)
            {
                counts[node] = 0;
                genMarker[node] = currentGeneration;
            }
            relt.onetomany[node][counts[node]++] = e;
            relt.maxnodenumber = std::max(relt.maxnodenumber, e);
        }
    }
}

void depthfirst(relationonetomany const &rel, size_t startnode, lst &order)
{
    assert(startnode < rel.maxnodenumber + 1);
    sek<bool> visited(rel.maxnodenumber + 1, false);
    std::stack<size_t> dfsStack;
    dfsStack.push(startnode);
    while (!dfsStack.empty())
    {
        size_t cur = dfsStack.top();
        dfsStack.pop();
        if (visited[cur])
            continue;
        visited[cur] = true;
        append(order, cur);
        if (cur < getsize(rel.onetomany))
            for (size_t i = 0, n = getsize(rel.onetomany[cur]); i < n; ++i)
            {
                size_t nbr = rel.onetomany[cur][i];
                if (!visited[nbr])
                    dfsStack.push(nbr);
            }
    }
}

void times(const relationonetomany &rela, const relationonetomany &relb, relationonetomany &relc)
{
    // Set the number of rows in the output equal to rela's row count.
    setnelem(relc, rela.nelem);
    relc.maxnodenumber = relb.maxnodenumber;
    // Use an integer marker array.
    // The marker stores the generation when an element was first encountered.
    sek<size_t> marker(relc.maxnodenumber + 1, 0);
    // We'll increment currentGeneration by 2 per row so that:
    // - currentGeneration is used for counting,
    // - currentGeneration+1 is used to indicate the element has been written.
    size_t currentGeneration = 1;

    for (size_t r = 0; r < rela.nelem; ++r)
    {
        size_t count = 0;

        // First pass: Iterate over rela's row and mark all unique b-elements from relb.
        for (size_t i = 0, nA = getsize(rela.onetomany[r]); i < nA; ++i)
        {
            size_t aCol = rela.onetomany[r][i];
            for (size_t j = 0, nB = getsize(relb.onetomany[aCol]); j < nB; ++j)
            {
                size_t bCol = relb.onetomany[aCol][j];
                if (marker[bCol] != currentGeneration)
                {
                    marker[bCol] = currentGeneration;
                    ++count;
                }
            }
        }
        // Allocate space in the output row.
        setsize(relc.onetomany[r], count);
        size_t idx = 0;
        // Second pass: Reiterate and copy the unique elements.
        // Use the fact that a value still marked with currentGeneration
        // has not yet been written. When written, update its marker to currentGeneration+1.
        for (size_t i = 0, nA = getsize(rela.onetomany[r]); i < nA; ++i)
        {
            size_t aCol = rela.onetomany[r][i];
            for (size_t j = 0, nB = getsize(relb.onetomany[aCol]); j < nB; ++j)
            {
                size_t bCol = relb.onetomany[aCol][j];
                if (marker[bCol] == currentGeneration)
                {
                    marker[bCol] = currentGeneration + 1;
                    relc.onetomany[r][idx++] = bCol;
                }
            }
        }

        // Move to the next pair of generations for the next row.
        currentGeneration += 2;
    }
}

void plusunion(relationonetomany const &a, relationonetomany const &b, relationonetomany &c)
{
    size_t maxelem = std::max(a.nelem, b.nelem);
    sek<size_t> iw;
    setnelem(c, maxelem);
    c.maxnodenumber = std::max(a.maxnodenumber, b.maxnodenumber);
    setsize(iw, c.maxnodenumber);
    for (size_t row = 0; row < maxelem; ++row)
    {
        size_t len{0};
        if (row < a.nelem)
        {
            for (size_t ka = 0; ka < getsize(a.onetomany[row]); ++ka)
            {
                size_t ca = a.onetomany[row][ka];
                len++;
                iw[ca] = len;
            }
        }
        if (row < b.nelem)
        {
            for (size_t kb = 0; kb < getsize(b.onetomany[row]); ++kb)
            {
                size_t cb = b.onetomany[row][kb];
                if (iw[cb] == 0)
                {
                    len++;
                    iw[cb] = len;
                }
            }
        }
        if (row < a.nelem)
        {
            for (size_t ka = 0; ka < getsize(a.onetomany[row]); ++ka)
            {
                iw[a.onetomany[row][ka]] = 0;
            }
        }
        if (row < b.nelem)
        {
            for (size_t kb = 0; kb < getsize(b.onetomany[row]); ++kb)
            {
                iw[b.onetomany[row][kb]] = 0;
            }
        }
        setsize(c.onetomany[row], len);
    }
    for (size_t row = 0; row < maxelem; ++row)
    {
        size_t len{0};
        if (row < a.nelem)
        {
            for (size_t ka = 0; ka < getsize(a.onetomany[row]); ++ka)
            {
                size_t ca = a.onetomany[row][ka];
                c.onetomany[row][len] = ca;
                len++;
                iw[ca] = len;
            }
        }
        if (row < b.nelem)
        {
            for (size_t kb = 0; kb < getsize(b.onetomany[row]); ++kb)
            {
                if (size_t cb = b.onetomany[row][kb]; iw[cb] == 0)
                {
                    c.onetomany[row][len] = cb;
                    len++;
                    iw[cb] = len;
                }
            }
        }
        if (row < a.nelem)
        {
            for (size_t ka = 0; ka < getsize(a.onetomany[row]); ++ka)
            {
                iw[a.onetomany[row][ka]] = 0;
            }
        }
        if (row < b.nelem)
        {
            for (size_t kb = 0; kb < getsize(b.onetomany[row]); ++kb)
            {
                iw[b.onetomany[row][kb]] = 0;
            }
        }
    }
}

void intersection(const relationonetomany &rela, const relationonetomany &relb, relationonetomany &relc)
{
    // Process rows up to the minimum number available.
    const size_t nRows = std::min(rela.nelem, relb.nelem);
    setnelem(relc, nRows);
    relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);

    // Use an integer marker array with a generation counter.
    std::vector<size_t> marker(relc.maxnodenumber + 1, 0);
    size_t currentGeneration = 1;

    for (size_t r = 0; r < nRows; ++r, ++currentGeneration)
    {
        // Mark the elements from relb's row using the current generation.
        const size_t relbSize = getsize(relb.onetomany[r]);
        for (size_t i = 0; i < relbSize; ++i)
        {
            marker[relb.onetomany[r][i]] = currentGeneration;
        }

        // First pass: Count elements in rela that are marked.
        size_t count = 0;
        const size_t relaSize = getsize(rela.onetomany[r]);
        for (size_t i = 0; i < relaSize; ++i)
        {
            if (marker[rela.onetomany[r][i]] == currentGeneration)
            {
                ++count;
            }
        }

        // Allocate the exact size for the intersection row.
        setsize(relc.onetomany[r], count);

        // Second pass: Copy the common elements directly into relc.
        size_t idx = 0;
        for (size_t i = 0; i < relaSize; ++i)
        {
            if (marker[rela.onetomany[r][i]] == currentGeneration)
            {
                relc.onetomany[r][idx++] = rela.onetomany[r][i];
            }
        }
    }
}

void difference(const relationonetomany &rela, const relationonetomany &relb, relationonetomany &relc)
{
    // Process rows up to the minimum number of rows in both relations.
    size_t nRows = std::min(rela.nelem, relb.nelem);
    setnelem(relc, nRows);
    relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);

    // Create a marker array that uses generations to avoid explicit resets.
    std::vector<int> marker(relc.maxnodenumber + 1, 0);
    int currentGeneration = 1;

    for (size_t r = 0; r < nRows; ++r, ++currentGeneration)
    {
        // Mark all nodes in the current row of relb with the current generation.
        for (auto node: relb.onetomany[r])
        {
            marker[node] = currentGeneration;
        }

        // First pass: count nodes in rela that are not in the current relb row.
        size_t count = 0;
        for (auto node: rela.onetomany[r])
        {
            if (marker[node] != currentGeneration)
            {
                ++count;
            }
        }

        // Allocate space for the output row.
        setsize(relc.onetomany[r], count);

        // Second pass: copy nodes from rela that are not marked.
        size_t idx = 0;
        for (auto node: rela.onetomany[r])
        {
            if (marker[node] != currentGeneration)
            {
                relc.onetomany[r][idx++] = node;
            }
        }
    }
}

void toporder(relationonetomany const &rel, lst &order)
{
    setsize(order, 0);
    std::vector<int> inDegree(getsize(rel.onetomany), 0);
    for (const auto &neighbors: rel.onetomany)
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
        for (const auto &nbr: rel.onetomany[cur])
            if (--inDegree[nbr] == 0)
                q.push(nbr);
    }
    if (getsize(order) != getsize(rel.onetomany))
        throw std::runtime_error("The relation contains cycles, topological sort not possible.");
}

void lexiorder(relationonetomany const &rel, lst &elemOrder) { elemOrder = getorder(rel.onetomany); }

void indicesfromorder(relationonetomany const &rel, const lst &elemOrder, lst &oldFromNew, lst &newFromOld)
{
    indicesfromorder(rel.onetomany, elemOrder, oldFromNew, newFromOld);
}

void compresselements(relationonetomany &rel, lst const &oldelementfromnew)
{
    rel.onetomany = rel.onetomany(oldelementfromnew);
    rel.nelem = getsize(oldelementfromnew);
    rel.maxnodenumber = 0;
    for (size_t elem = 0; elem < rel.nelem; ++elem)
    {
        for (size_t node = 0; node < getsize(rel.onetomany[elem]); ++node)
        {
            rel.maxnodenumber = std::max(rel.maxnodenumber, rel.onetomany[elem][node]);
        }
    }
}

void compressnodes(relationonetomany &rel, lst const &newnodefromold)
{
    for (size_t i = 0; i < getsize(rel.onetomany); ++i)
        rel.onetomany[i] = newnodefromold(rel.onetomany[i]);
    for (size_t elem = 0; elem < rel.nelem; ++elem)
    {
        for (size_t node = 0; node < getsize(rel.onetomany[elem]); ++node)
        {
            rel.maxnodenumber = std::max(rel.maxnodenumber, rel.onetomany[elem][node]);
        }
    }
}
