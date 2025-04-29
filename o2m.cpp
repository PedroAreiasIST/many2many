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
    inline int update_max_for_nodes(seque<int> const &nodes, int current_max)
    {
        return std::reduce(nodes.begin(), nodes.end(), current_max,
                           [](int a, int b) { return std::max(a, b); });
    }

    inline int update_max_for_nodes(seque<seque<int> > const &lnods)
    {
        int current_max = -1;
        for (int i = 0; i < getsize(lnods); ++i)
        {
            current_max =
                    std::max(current_max, update_max_for_nodes(lnods[i], current_max));
        }
        return current_max;
    }
} // namespace hidden
inline o2m geto2mfromsequence(const seque<int> &other)
{
    o2m ret;
    setsize(ret, getsize(other));
    for (int element = 0; element < getsize(other); ++element)
    {
        ret.lnods[element] = {element};
    }
    return ret;
}

seque<seque<int> > getcliques(const o2m &nodesfromelement,
                              const o2m &elementsfromnode)
{
    seque<seque<int> > cliques;
    setsize(cliques, nodesfromelement.nelems());

    seque<seque<int> > nodelocation =
            hidden::getnodepositions(nodesfromelement, elementsfromnode);

    for (int element = 0; element < nodesfromelement.nelems(); ++element)
    {
        int ns = getsize(nodesfromelement[element]);
        setsize(cliques[element], ns * ns);
    }

#pragma omp parallel
    {
        seque<int> local_marker(elementsfromnode.nelems(), 0);
        seque<int> local_markerGen(elementsfromnode.nelems(), -1);

#pragma omp for schedule(static)
        for (int node1 = 0; node1 < elementsfromnode.nelems(); ++node1)
        {
            int generation = node1 + 1;
            int nnz = 0;

            for (int lelement = 0; lelement < elementsfromnode.nnodes(node1);
                 ++lelement)
            {
                int lnode1 = nodelocation[node1][lelement];
                int element = elementsfromnode[node1][lelement];
                int esize = nodesfromelement.nnodes(element);

                for (int lnode2 = 0; lnode2 < nodesfromelement.nnodes(element);
                     ++lnode2)
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
            }
        }
    }
    return cliques;
}

void setsize(o2m &rel, int nelem)
{
    rel.nelem = nelem;
    setsize(rel.lnods, nelem);
}

void setnodesforelement(o2m &rel, int element, seque<int> const &nodes)
{
    if (element >= rel.nelems())
        throw std::runtime_error("Element index out of bounds");
    erase(rel.lnods[element]);
    setsize(rel.lnods[element], getsize(nodes));
    std::copy(std::execution::par, nodes.begin(), nodes.end(),
              rel.lnods[element].begin());
    if (getsize(rel.lnods[element]) != getsize(nodes))
    {
        rel.lnods[element] = nodes;
    } else
        std::copy(std::execution::par, nodes.begin(), nodes.end(),
                  rel.lnods[element].begin());
    rel.maxnode = hidden::update_max_for_nodes(nodes, rel.maxnode);
}

int appendelement(o2m &rel, seque<int> const &nodes)
{
    seque<int> nodetemp = nodes;
    setorderedandunique(nodetemp);
    if (getsize(nodetemp) != getsize(nodes))
    {
        throw std::runtime_error("Repeated nodes detected in the input.");
    }
    rel.nelem++;
    setsize(rel.lnods, rel.nelem);
    assert(getsize(rel.lnods) == rel.nelem);
    rel.lnods[rel.nelem - 1] = nodes;
    rel.maxnode = hidden::update_max_for_nodes(nodes, rel.maxnode);
    return rel.nelem - 1;
}

o2m &operator<<(o2m &rel, std::initializer_list<int> nodes)
{
    seque<int> temp(nodes.size());
    for (auto i = 0; i < nodes.size(); ++i)
    {
        temp[i] = *(nodes.begin() + i);
    }
    appendelement(rel, temp);
    return rel;
}

o2m Tr(const o2m &rel)
{
    o2m relt;
    relt.nelem = 0;
    relt.maxnode = 0;
    relt.lnods = {{}};
    if (rel.nelem == 0)
    {
        return relt;
    }

    relt.maxnode = rel.nelem - 1;
    const int numberOfNodes = rel.maxnode + 1;

    std::vector<int> counts(numberOfNodes, 0);

#pragma omp parallel for schedule(static)
    for (int elementIdx = 0; elementIdx < rel.nelem; ++elementIdx)
    {
        for (int nodeId: rel.lnods[elementIdx])
        {
#pragma omp atomic
            counts[nodeId]++;
        }
    }

    setsize(relt, numberOfNodes);

    for (int node = 0; node < numberOfNodes; ++node)
    {
        setsize(relt.lnods[node], counts[node]);
    }

    std::vector<int> offsets(numberOfNodes, 0);

    for (int elementIndex = 0; elementIndex < rel.nelem; ++elementIndex)
    {
        for (int node: rel.lnods[elementIndex])
        {
            relt.lnods[node][offsets[node]++] = elementIndex;
        }
    }

    return relt;
}

o2m operator*(const o2m &rela, const o2m &relb)
{
    o2m relc;
    const int camax = relb.nelems() - 1;
    relc.maxnode = relb.maxnode;
    setsize(relc, rela.nelems());

    std::vector<int> row_sizes(rela.nelems(), 0);
    if (rela.maxnode <= camax)
    {
#pragma omp parallel
        {
            seque<int> marker(relc.maxnode + 1, 0);
            int generation = 0;

#pragma omp for schedule(dynamic)
            for (int ra = 0; ra < rela.nelems(); ++ra)
            {
                generation++;
                int len = 0;
                for (int ka = 0; ka < rela.nnodes(ra); ++ka)
                {
                    int ca = rela[ra][ka];
                    for (int kb = 0; kb < relb.nnodes(ca); ++kb)
                    {
                        int cb = relb[ca][kb];
                        if (marker[cb] != generation)
                        {
                            marker[cb] = generation;
                            len++;
                        }
                    }
                }
                row_sizes[ra] = len;
            }
        }

        for (int ra = 0; ra < rela.nelems(); ++ra)
            setsize(relc.lnods[ra], row_sizes[ra]);

#pragma omp parallel
        {
            seque<int> marker(relc.maxnode + 1, 0);
            int generation = 0;

#pragma omp for schedule(dynamic)
            for (int ra = 0; ra < rela.nelems(); ++ra)
            {
                generation++;
                int len = 0;
                for (int ka = 0; ka < rela.nnodes(ra); ++ka)
                {
                    int ca = rela[ra][ka];
                    for (int kb = 0; kb < relb.nnodes(ca); ++kb)
                    {
                        int cb = relb[ca][kb];
                        if (marker[cb] != generation)
                        {
                            marker[cb] = generation;
                            relc[ra][len++] = cb;
                        }
                    }
                }
            }
        }
    } else
    {
#pragma omp parallel
        {
            seque<int> marker(relc.maxnode + 1, 0);
            int generation = 0;

#pragma omp for schedule(dynamic)
            for (int ra = 0; ra < rela.nelems(); ++ra)
            {
                generation++;
                int len = 0;
                for (int ka = 0; ka < rela.nnodes(ra); ++ka)
                {
                    int ca = rela[ra][ka];
                    if (ca > camax)
                        continue;
                    for (int kb = 0; kb < relb.nnodes(ca); ++kb)
                    {
                        int cb = relb[ca][kb];
                        if (marker[cb] != generation)
                        {
                            marker[cb] = generation;
                            len++;
                        }
                    }
                }
                row_sizes[ra] = len;
            }
        }

        for (int ra = 0; ra < rela.nelems(); ++ra)
            setsize(relc.lnods[ra], row_sizes[ra]);

#pragma omp parallel
        {
            seque<int> marker(relc.maxnode + 1, 0);
            int generation = 0;

#pragma omp for schedule(dynamic)
            for (int ra = 0; ra < rela.nelems(); ++ra)
            {
                generation++;
                int len = 0;
                for (int ka = 0; ka < rela.nnodes(ra); ++ka)
                {
                    int ca = rela[ra][ka];
                    if (ca > camax)
                        continue;
                    for (int kb = 0; kb < relb.nnodes(ca); ++kb)
                    {
                        int cb = relb[ca][kb];
                        if (marker[cb] != generation)
                        {
                            marker[cb] = generation;
                            relc[ra][len++] = cb;
                        }
                    }
                }
            }
        }
    }

    return relc;
}

o2m operator*(const o2m &rela, const seque<int> &vec)
{
    o2m temp = geto2mfromsequence(vec);
    return rela * temp;
}

o2m operator+(const o2m &rela, const o2m &relb)
{
    o2m relc;
    int maxElements = std::max(rela.nelem, relb.nelem);
    setsize(relc, maxElements);
    relc.maxnode = std::max(rela.maxnode, relb.maxnode); {
        std::vector<int> marker(relc.maxnode + 1, 0);
        int local_generation = 1;
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
    return relc;
}

o2m operator||(const o2m &rela, const o2m &relb) { return rela + relb; }

o2m operator&&(const o2m &rela, const o2m &relb)
{
    o2m relc;
    const int nElements = std::min(rela.nelem, relb.nelem);
    setsize(relc, nElements);
    relc.maxnode = std::max(rela.maxnode, relb.maxnode);
#pragma omp parallel for schedule(static)
    for (int element = 0; element < nElements; ++element)
    {
        seque<int> elementB = relb.lnods[element];
        seque<int> elementA = rela.lnods[element];
        setordered(elementB);
        setordered(elementA);
        relc.lnods[element] = std::move(getintersection(elementA, elementB));
    }
    return relc;
}

o2m operator-(const o2m &rela, const o2m &relb)
{
    o2m relc;
    const int nElements = rela.nelem;
    setsize(relc, nElements);
    relc.maxnode = std::max(rela.maxnode, relb.maxnode);
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
    return relc;
}

seque<int> gettoporder(const o2m &rel)
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

seque<int> getorder(const o2m &rel) { return getorder(rel.lnods); }

void hidden::compresselements(o2m &rel, const seque<int> &oldelementfromnew)
{
    rel.lnods = rel.lnods(oldelementfromnew);
    rel.nelem = getsize(oldelementfromnew);
    rel.maxnode = hidden::update_max_for_nodes(rel.lnods);
}

void hidden::permutenodes(o2m &rel, const seque<int> &newnodefromold)
{
#pragma omp parallel for schedule(dynamic)
    for (int i = 0; i < getsize(rel.lnods); ++i)
    {
        rel.lnods[i] = newnodefromold(rel.lnods[i]);
    }
    rel.maxnode = hidden::update_max_for_nodes(rel.lnods);
}

seque<seque<int> > hidden::getnodepositions(o2m const &nodesfromelement,
                                            o2m const &elementsfromnode)
{
    seque<seque<int> > nodepositions(elementsfromnode.nelems());

#pragma omp parallel for schedule(static)
    for (int node = 0; node < elementsfromnode.nelems(); ++node)
    {
        setsize(nodepositions[node], elementsfromnode.nnodes(node));
    }

    seque<int> nodePositionCounter(elementsfromnode.nelems(), 0);

#pragma omp parallel for schedule(dynamic)
    for (int element = 0; element < nodesfromelement.nelems(); ++element)
    {
        const auto &nodes = nodesfromelement.lnods[element];
        int ns = getsize(nodes);

        for (int localPosition = 0; localPosition < ns; ++localPosition)
        {
            int node = nodes[localPosition];
            int index;

#pragma omp atomic capture
            {
                index = nodePositionCounter[node];
                nodePositionCounter[node]++;
            }

            nodepositions[node][index] = localPosition;
        }
    }
    return nodepositions;
}

seque<seque<int> > hidden::getelementpositions(o2m const &nodesfromelement,
                                               o2m const &elementsfromnode)
{
    seque<seque<int> > elementpositions(nodesfromelement.nelems());

#pragma omp parallel for schedule(static)
    for (int element = 0; element < nodesfromelement.nelems(); ++element)
    {
        int count = nodesfromelement.nnodes(element);
        setsize(elementpositions[element], count);
    }

#pragma omp parallel for schedule(dynamic)
    for (int node = 0; node < elementsfromnode.nelems(); ++node)
    {
        const auto &elemList = elementsfromnode.lnods[node];
        int numElems = getsize(elemList);
        for (int pos = 0; pos < numElems; ++pos)
        {
            int element = elemList[pos];
            const auto &nodes = nodesfromelement.lnods[element];
            int ns = getsize(nodes);
            for (int i = 0; i < ns; ++i)
            {
                if (nodes[i] == node)
                {
                    elementpositions[element][i] = pos;
                    break;
                }
            }
        }
    }
    return elementpositions;
}
