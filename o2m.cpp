#include "o2m.hpp"
#include <algorithm>
#include <cassert>
#include <execution>
#include <numeric>
#include <queue>
#include <stdexcept>
#include <vector>

namespace hidden
{
    int update_max_for_nodes(seque<int> const &nodes, int current_max)
    {
        return std::reduce(std::execution::par, nodes.begin(), nodes.end(), current_max,
                           [](int a, int b) { return std::max(a, b); });
    }
} // namespace hidden
o2m convertfromlist(const seque<int> &other)
{
    o2m ret;
    setsize(ret, getsize(other));
#pragma omp parallel for schedule(static)
    for (int element = 0; element < ret.nelem; ++element)
    {
        setnodesforelement(ret, element, {other[element]});
    }
    return ret;
}

seque<seque<int> > getcliqueaddressing(const o2m &nodesfromelement, const o2m &elementsfromnode)
{
    seque<seque<int> > cliques;
    setsize(cliques, nodesfromelement.size());
    seque<seque<int> > nodelocation = getnodepositions(nodesfromelement, elementsfromnode);
    for (int element = 0; element < nodesfromelement.size(); ++element)
    {
        int ns = getsize(nodesfromelement[element]);
        setsize(cliques[element], ns * ns);
    }

    int generation = 1;
    // Allocate local marker arrays for each node1 iteration.
    // These have the full size of all nodes.
    seque<int> local_marker(elementsfromnode.size(), 0);
    seque<int> local_markerGen(elementsfromnode.size(), -1);
    // Parallelize over node1. Each iteration gets its own local scratch space.
    for (int node1 = 0; node1 < elementsfromnode.size(); ++node1)
    {
        // Initialize per-iteration scratch variables.
        int nnz(0);
        // For each occurrence of node1 in the connected elements.
        for (int lelement = 0; lelement < elementsfromnode.size(node1); ++lelement)
        {
            int lnode1 = nodelocation[node1][lelement];
            int element = elementsfromnode[node1][lelement];
            int esize = nodesfromelement.size(element);
            for (int lnode2 = 0; lnode2 < nodesfromelement.size(element); ++lnode2)
            {
                int node2 = nodesfromelement[element][lnode2];
                if (local_markerGen[node2] != generation)
                {
                    local_markerGen[node2] = generation;
                    local_marker[node2] = nnz;
                    cliques[element][lnode2 + lnode1 * esize] = nnz;
                    nnz++;
                } else
                {
                    cliques[element][lnode2 + lnode1 * esize] = local_marker[node2];
                }
            }
            // Instead of clearing the marker array, we increment the generation counter.
        }
        generation++;
    }
    return cliques;
}

void setsize(o2m &rel, int nelem)
{
    rel.nelem = nelem;
    setsize(rel.lnods, nelem);
}

void setsizes(o2m &rel, seque<int> const &sizes)
{
    int sm = std::min(getsize(sizes), rel.size());
    for (int element = 0; element < sm; ++element)
    {
        setsize(rel[element], sizes[element]);
    }
}

void setnodesforelement(o2m &rel, int element, seque<int> const &nodes)
{
    if (getsize(rel.lnods[element]) == 0)
    {
        rel.lnods[element] = nodes;
    } else
        std::copy(std::execution::par, nodes.begin(), nodes.end(), rel.lnods[element].begin());
    rel.maxnodenumber = hidden::update_max_for_nodes(nodes, rel.maxnodenumber);
}

void setnodesforelement(o2m &rel, int element, seque<int> &&nodes)
{
    rel.lnods[element] = std::move(nodes);
    rel.maxnodenumber = hidden::update_max_for_nodes(nodes, rel.maxnodenumber);
}

int appendelement(o2m &rel, seque<int> const &nodes)
{
    rel.nelem++;
    setsize(rel.lnods, rel.nelem);
    assert(getsize(rel.lnods) == rel.nelem);
    rel.lnods[rel.nelem - 1] = nodes;
    rel.maxnodenumber = hidden::update_max_for_nodes(nodes, rel.maxnodenumber);
    return rel.nelem - 1;
}

o2m Tr(const o2m &rel)
{
    o2m relt;
    // Return immediately for empty relation.
    if (rel.nelem == 0)
    {
        return relt;
    }

    // Assume that the maximum node number in the transposed relation
    // equals the number of elements from the original relation.
    relt.maxnodenumber = rel.nelem - 1;
    const int numberOfNodes = rel.maxnodenumber + 1;

    // Use vector instead of seque for counts and offsets to avoid potential
    // overhead
    std::vector<int> counts(numberOfNodes, 0);

    // Count occurrences of nodes per element
#pragma omp parallel for schedule(static)
    for (int elementIdx = 0; elementIdx < rel.nelem; ++elementIdx)
    {
        for (int nodeId: rel.lnods[elementIdx])
        {
#pragma omp atomic
            counts[nodeId]++;
        }
    }

    // Set the number of elements in the transposed relation
    setsize(relt, numberOfNodes);

    // Allocate node lists in the transposed relation
    for (int node = 0; node < numberOfNodes; ++node)
    {
        setsize(relt.lnods[node], counts[node]);
    }

    // Reuse the counts array as offsets to avoid unnecessary memory allocations
    std::vector<int> offsets(numberOfNodes, 0);

    // Fill in the transposed relation
    for (int elementIndex = 0; elementIndex < rel.nelem; ++elementIndex)
    {
        for (int node: rel.lnods[elementIndex])
        {
            relt.lnods[node][offsets[node]++] = elementIndex;
        }
    }

    return relt;
}

o2m multiplication(const o2m &rela, const o2m &relb)
{
    o2m relc;
    relc.maxnodenumber = relb.maxnodenumber;
    // Preallocate the outer container for relc with the same number of rows as rela.
    setsize(relc, rela.size());

    // First pass: Determine the unique counts per row.
    // We'll use a temporary vector to store the count of unique elements for each row.
    std::vector<int> uniqueCounts(rela.size(), 0);

    // Parallelize the first pass over rows.
#pragma omp parallel for
    for (int ra = 0; ra < static_cast<int>(rela.size()); ++ra)
    {
        // Allocate a local marker (vector of bool) for this row.
        // Initialized to false, meaning not yet seen.
        std::vector<bool> marker(relc.maxnodenumber + 1, false);
        int count = 0;
        for (int ka = 0; ka < rela.size(ra); ++ka)
        {
            int ca = rela[ra][ka];
            for (int kb = 0; kb < relb.size(ca); ++kb)
            {
                int cb = relb[ca][kb];
                if (!marker[cb])
                {
                    marker[cb] = true;
                    ++count;
                }
            }
        }
        uniqueCounts[ra] = count;
    }

    // Now that we know the required size of each row in relc,
    // allocate space for each row in relc.
#pragma omp parallel for
    for (int ra = 0; ra < static_cast<int>(rela.size()); ++ra)
    {
        setsize(relc.lnods[ra], uniqueCounts[ra]);
    }

    // Second pass: Build the relation by filling each row.
#pragma omp parallel for
    for (int ra = 0; ra < static_cast<int>(rela.size()); ++ra)
    {
        // Again, allocate a local marker for the current row.
        std::vector<bool> marker(relc.maxnodenumber + 1, false);
        int pos = 0; // Output index within the row.
        for (int ka = 0; ka < rela.size(ra); ++ka)
        {
            int ca = rela[ra][ka];
            for (int kb = 0; kb < relb.size(ca); ++kb)
            {
                int cb = relb[ca][kb];
                if (!marker[cb])
                {
                    marker[cb] = true;
                    relc[ra][pos] = cb;
                    ++pos;
                }
            }
        }
    }
    return relc;
}

o2m operator*(const o2m &rela, const o2m &relb)
{
    return multiplication(rela, relb);
}

o2m operator*(const o2m &rela, const seque<int> &vec)
{
    return multiplication(rela, convertfromlist(vec));
}

void addition(const o2m &rela, const o2m &relb, o2m &relc)
{
    int maxElements = std::max(rela.nelem, relb.nelem);
    setsize(relc, maxElements);
    relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
#pragma omp parallel
    {
        std::vector<int> marker(relc.maxnodenumber + 1, 0);
        int local_generation = 1;
#pragma omp for schedule(static)
        for (int element = 0; element < maxElements; ++element)
        {
            int count = 0;
            if (element < rela.nelem)
            {
                const seque<int> &elementA = rela.lnods[element];
                for (int node: elementA)
                {
                    if (marker[node] != local_generation)
                    {
                        marker[node] = local_generation;
                        count++;
                    }
                }
            }
            if (element < relb.nelem)
            {
                const seque<int> &elementB = relb.lnods[element];
                for (int node: elementB)
                {
                    if (marker[node] != local_generation)
                    {
                        marker[node] = local_generation;
                        count++;
                    }
                }
            }
            seque<int> temp(count);
            count = 0;
            if (element < rela.nelem)
            {
                const seque<int> &elementA = rela.lnods[element];
                for (int node: elementA)
                {
                    if (marker[node] == local_generation)
                    {
                        temp[count++] = node;
                        marker[node] = local_generation + 1;
                    }
                }
            }
            if (element < relb.nelem)
            {
                const seque<int> &elementB = relb.lnods[element];
                for (int node: elementB)
                {
                    if (marker[node] == local_generation)
                    {
                        temp[count++] = node;
                        marker[node] = local_generation + 1;
                    }
                }
            }
            relc.lnods[element] = std::move(temp);
            local_generation += 2;
        }
    }
}

o2m operator+(const o2m &rela, const o2m &rel)
{
    o2m relc;
    addition(rela, rel, relc);
    return relc;
}

o2m operator||(const o2m &a, const o2m &b)
{
    o2m c;
    addition(a, b, c);
    return c;
}

o2m operator&&(const o2m &a, const o2m &b)
{
    o2m c;
    intersection(a, b, c);
    return c;
}

void intersection(const o2m &rela, const o2m &relb, o2m &relc)
{
    const int nElements = std::min(rela.nelem, relb.nelem);
    setsize(relc, nElements);
    relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
#pragma omp parallel for schedule(static)
    for (int element = 0; element < nElements; ++element)
    {
        seque<int> elementB = relb.lnods[element];
        seque<int> elementA = rela.lnods[element];
        setordered(elementB);
        setordered(elementA);
        relc.lnods[element] = std::move(getintersection(elementA, elementB));
    }
}

void subtraction(const o2m &rela, const o2m &relb, o2m &relc)
{
    const int nElements = rela.nelem;
    setsize(relc, nElements);
    relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
#pragma omp parallel for schedule(static)
    for (int element = 0; element < nElements; ++element)
    {
        if (element < relb.nelem)
        {
            seque<int> elementB = relb.lnods[element];
            seque<int> elementA = rela.lnods[element];
            setordered(elementB);
            setordered(elementA);
            relc.lnods[element] = std::move(getdifference(elementA, elementB));
        } else
        {
            relc.lnods[element] = std::move(rela.lnods[element]);
        }
    }
}

o2m operator-(const o2m &rela, const o2m &relb)
{
    o2m relc;
    subtraction(rela, relb, relc);
    return relc;
}

seque<int> toporder(const o2m &rel)
{
    seque<int> order;
    setsize(order, 0);
    std::vector<int> inDegree(getsize(rel.lnods), 0);
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
    for (int i = 0; i < getsize(rel.lnods); ++i)
    {
        seque<int> const &element = rel.lnods[i];
        for (int node: element)
        {
#ifdef _OPENMP
#pragma omp atomic
#endif
            ++inDegree[node];
        }
    }
    std::queue<int> q;
    for (int i = 0; i < inDegree.size(); ++i)
        if (inDegree[i] == 0)
            q.push(i);
    while (!q.empty())
    {
        int cur = q.front();
        q.pop();
        append(order, cur);
        for (int nbr: rel.lnods[cur])
        {
            if (--inDegree[nbr] == 0)
                q.push(nbr);
        }
    }
    if (getsize(order) != getsize(rel.lnods))
        throw std::runtime_error(
            "The relation contains cycles, topological sort not possible.");
    return order;
}

seque<int> lexiorder(const o2m &rel) { return getorder(rel.lnods); }

void indicesfromorder(const o2m &rel, const seque<int> &elemOrder,
                      seque<int> &oldFromNew, seque<int> &newFromOld)
{
    indicesfromorder(rel.lnods, elemOrder, oldFromNew, newFromOld);
}

void compresselements(o2m &rel, const seque<int> &oldelementfromnew)
{
    rel.lnods = rel.lnods(oldelementfromnew);
    rel.nelem = getsize(oldelementfromnew);
    int local_max = std::transform_reduce(
        std::execution::par, rel.lnods.begin(), rel.lnods.end(), int(0),
        [](int a, int b) { return std::max(a, b); },
        [](seque<int> const &element)
        {
            return std::reduce(std::execution::par, element.begin(), element.end(),
                               int(0), [](int a, int b) { return std::max(a, b); });
        });
    rel.maxnodenumber = local_max;
}

void permutenodes(o2m &rel, const seque<int> &newnodefromold)
{
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
    for (int i = 0; i < getsize(rel.lnods); ++i)
    {
        rel.lnods[i] = newnodefromold(rel.lnods[i]);
    }
    int local_max = std::transform_reduce(
        std::execution::par, rel.lnods.begin(), rel.lnods.end(), int(0),
        [](int a, int b) { return std::max(a, b); },
        [](seque<int> const &element)
        {
            return std::reduce(std::execution::par, element.begin(), element.end(),
                               int(0), [](int a, int b) { return std::max(a, b); });
        });
    rel.maxnodenumber = local_max;
}

seque<seque<int> > getnodepositions(o2m const &nodesfromelement, o2m const &elementsfromnode)
{
    // Create a lookup table: for each global node, store the local positions
    // at which it appears in various elements. The size of the outer sequence is
    // the number of nodes in the global context.
    seque<seque<int> > nodepositions(elementsfromnode.size());

    // Parallel allocation: For each node, allocate storage based on how many elements the node is part of.
#pragma omp parallel for schedule(static)
    for (int node = 0; node < elementsfromnode.size(); ++node)
    {
        setsize(nodepositions[node], elementsfromnode.size(node));
    }

    // Prepare a counter for each node to track where the next insertion should occur.
    // Initialized to 0 for each node.
    seque<int> nodePositionCounter(elementsfromnode.size(), 0);

    // Parallelize the outer loop over elements.
#pragma omp parallel for schedule(dynamic)
    for (int element = 0; element < nodesfromelement.size(); ++element)
    {
        // Retrieve the current element's node list.
        const auto &nodes = nodesfromelement.lnods[element];
        int ns = getsize(nodes); // Cache number of nodes for this element.

        // Process each local position in the element.
        for (int localPosition = 0; localPosition < ns; ++localPosition)
        {
            int node = nodes[localPosition];
            int index;

            // Atomically capture the current counter value for this node and increment it.
#pragma omp atomic capture
            {
                index = nodePositionCounter[node];
                nodePositionCounter[node]++;
            }

            // Write the local position into the appropriate slot.
            nodepositions[node][index] = localPosition;
        }
    }
    return nodepositions;
}

seque<seque<int> > getelementpositions(o2m const &nodesfromelement,
                                       o2m const &elementsfromnode)
{
    // Allocate the output lookup table:
    // For each element, we create a vector with one entry per node in that element.
    seque<seque<int> > elementpositions(nodesfromelement.size());

    // Parallelize allocation: Each element's position vector is allocated independently.
#pragma omp parallel for schedule(static)
    for (int element = 0; element < nodesfromelement.size(); ++element)
    {
        int count = nodesfromelement.size(element);
        setsize(elementpositions[element], count);
    }

    // Parallelize over all global nodes.
#pragma omp parallel for schedule(dynamic)
    for (int node = 0; node < elementsfromnode.size(); ++node)
    {
        const auto &elemList = elementsfromnode.lnods[node];
        int numElems = getsize(elemList);
        // For each element that contains this node...
        for (int pos = 0; pos < numElems; ++pos)
        {
            int element = elemList[pos];
            // Retrieve the node list of the element.
            const auto &nodes = nodesfromelement.lnods[element];
            int ns = getsize(nodes);
            // Find the local position within this element at which 'node' appears.
            for (int i = 0; i < ns; ++i)
            {
                if (nodes[i] == node)
                {
                    // Save the position 'pos' for this element at the matching local index.
                    elementpositions[element][i] = pos;
                    break; // Move to the next element since the node has been found.
                }
            }
        }
    }
    return elementpositions;
}
