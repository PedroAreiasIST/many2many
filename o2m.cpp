#include "o2m.hpp"
#include <algorithm>
#include <cassert>
#include <cstring> // For std::memcpy
#include <execution>
#include <numeric>
#include <omp.h>
#include <queue>
#include <stdexcept>
#include <vector>
namespace hidden {
int update_max_for_nodes(seque<int> const &nodes, int current_max) {
  return std::reduce(nodes.begin(), nodes.end(), current_max,
                     [](int a, int b) { return std::max(a, b); });
}
} // namespace hidden
o2m convertfromlist(const seque<int> &other) {
  o2m ret;
  setnumberofelements(ret, getsize(other));
  for (int element = 0; element < ret.nelem; ++element) {
    setnodesforelement(ret, element, {other[element]});
  }
  return ret;
}
void setnumberofelements(o2m &rel, int nelem) {
  rel.nelem = nelem;
  setsize(rel.lnods, nelem);
}
void setnodesforelement(o2m &rel, int element, seque<int> const &nodes) {
  rel.lnods[element] = nodes;
  rel.maxnodenumber = hidden::update_max_for_nodes(nodes, rel.maxnodenumber);
}

void setnodesforelement(o2m &rel, int element, seque<int> &&nodes) {
  rel.lnods[element] = std::move(nodes);
  rel.maxnodenumber = hidden::update_max_for_nodes(nodes, rel.maxnodenumber);
}

int appendelement(o2m &rel, seque<int> const &nodes) {
  rel.nelem++;
  setsize(rel.lnods, rel.nelem);
  assert(getsize(rel.lnods) == rel.nelem);
  rel.lnods[rel.nelem - 1] = nodes;
  rel.maxnodenumber = hidden::update_max_for_nodes(nodes, rel.maxnodenumber);
  return rel.nelem - 1;
}

o2m transpose(const o2m &rel) {
  o2m relt;

  // Return immediately for empty relation.
  if (rel.nelem == 0) {
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
  for (int elementIdx = 0; elementIdx < rel.nelem; ++elementIdx) {
    for (int nodeId : rel.lnods[elementIdx]) {
#pragma omp atomic
      counts[nodeId]++;
    }
  }

  // Set the number of elements in the transposed relation
  setnumberofelements(relt, numberOfNodes);

  // Allocate node lists in the transposed relation
  for (int node = 0; node < numberOfNodes; ++node) {
    setsize(relt.lnods[node], counts[node]);
  }

  // Reuse the counts array as offsets to avoid unnecessary memory allocations
  std::vector<int> offsets(numberOfNodes, 0);

  // Fill in the transposed relation
  for (int elementIndex = 0; elementIndex < rel.nelem; ++elementIndex) {
    for (int node : rel.lnods[elementIndex]) {
      relt.lnods[node][offsets[node]++] = elementIndex;
    }
  }

  return relt;
}

void multiplication(const o2m &rel, const seque<int> &vec, o2m &vecresult) {
  multiplication(rel, convertfromlist(vec), vecresult);
}

void multiplication(const o2m &rela, const o2m &relb, o2m &relc) {
  // Configure the output container for "relc" based on the input "rela".
  setnumberofelements(relc, rela.nelem);
  if (rela.nelem == 0 || relb.maxnodenumber == 0)
    return;
  relc.maxnodenumber = relb.maxnodenumber;

#pragma omp parallel
  {
    // Thread-local visited marker array: one byte per possible node.
    // A value of 0 means “not seen” in the current iteration.
    seque<char> visited(relb.maxnodenumber + 1, 0);

    // Container to record each unique node ID encountered in pass one.
    seque<int> touchedNodes;

    // A container for the final result. We’ll allocate it exactly
    // to the number of unique nodes found.
    seque<int> resultNodes;

#pragma omp for schedule(static)
    for (int elementAIndex = 0; elementAIndex < rela.nelem; ++elementAIndex) {
      const seque<int> &elementA = rela.lnods[elementAIndex];

      // --- Pass One: Mark Unique Nodes and Record Their IDs ---
      // Clear any remnants from prior iterations.
      erase(touchedNodes);

      for (int nodeA : elementA) {
        // Only consider valid indices.
        if (nodeA >= relb.nelem)
          continue;
        const seque<int> &elementB = relb.lnods[nodeA];
        int bSize = getsize(elementB);

        for (int j = 0; j < bSize; ++j) {
#if defined(__GNUC__) || defined(__clang__)
          if (j + 1 < bSize)
            __builtin_prefetch(&elementB[j + 1], 0, 3);
#endif
          int nodeB = elementB[j];
          // If not visited, mark it and record its ID.
          if (visited[nodeB] == 0) {
            visited[nodeB] = 1;
            append(touchedNodes, nodeB);
          }
        }
      }

      // --- Pass Two: Build the Output Container ---
      // Now we know exactly how many unique nodes we have.
      setsize(resultNodes, touchedNodes.size);
      for (int i = 0; i < touchedNodes.size; ++i) {
        resultNodes[i] = touchedNodes[i];
      }

      // --- Cleanup: Reset the visited Flags ---
      // Instead of scanning the entire "visited" array, we only reset the nodes
      // touched.
      for (int nodeB : touchedNodes) {
        visited[nodeB] = 0;
      }

      // Save the exact result for this element.
      relc[elementAIndex] = resultNodes;
    }
  }
}

o2m operator*(const o2m &rela, const o2m &relb) {
  o2m relc;
  multiplication(rela, relb, relc);
  return relc;
}

o2m operator*(const o2m &rela, const seque<int> &vec) {
  o2m relc;
  multiplication(rela, vec, relc);
  return relc;
}

void addition(const o2m &rela, const o2m &relb, o2m &relc) {
  int maxElements = std::max(rela.nelem, relb.nelem);
  setnumberofelements(relc, maxElements);
  relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
#pragma omp parallel
  {
    std::vector<int> marker(relc.maxnodenumber + 1, 0);
    int local_generation = 1;
#pragma omp for schedule(static)
    for (int element = 0; element < maxElements; ++element) {
      int count = 0;
      if (element < rela.nelem) {
        const seque<int> &elementA = rela.lnods[element];
        for (int node : elementA) {
          if (marker[node] != local_generation) {
            marker[node] = local_generation;
            count++;
          }
        }
      }
      if (element < relb.nelem) {
        const seque<int> &elementB = relb.lnods[element];
        for (int node : elementB) {
          if (marker[node] != local_generation) {
            marker[node] = local_generation;
            count++;
          }
        }
      }
      seque<int> temp(count);
      count = 0;
      if (element < rela.nelem) {
        const seque<int> &elementA = rela.lnods[element];
        for (int node : elementA) {
          if (marker[node] == local_generation) {
            temp[count++] = node;
            marker[node] = local_generation + 1;
          }
        }
      }
      if (element < relb.nelem) {
        const seque<int> &elementB = relb.lnods[element];
        for (int node : elementB) {
          if (marker[node] == local_generation) {
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
o2m operator+(const o2m &rela, const o2m &rel) {
  o2m relc;
  addition(rela, rel, relc);
  return relc;
}
o2m operator||(const o2m &a, const o2m &b) {
  o2m c;
  addition(a, b, c);
  return c;
}
o2m operator&&(const o2m &a, const o2m &b) {
  o2m c;
  intersection(a, b, c);
  return c;
}
void intersection(const o2m &rela, const o2m &relb, o2m &relc) {
  const int nElements = std::min(rela.nelem, relb.nelem);
  setnumberofelements(relc, nElements);
  relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
#pragma omp parallel for schedule(static)
  for (int element = 0; element < nElements; ++element) {
    seque<int> elementB = relb.lnods[element];
    seque<int> elementA = rela.lnods[element];
    setordered(elementB);
    setordered(elementA);
    relc.lnods[element] = std::move(getintersection(elementA, elementB));
  }
}
void subtraction(const o2m &rela, const o2m &relb, o2m &relc) {
  const int nElements = rela.nelem;
  setnumberofelements(relc, nElements);
  relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
#pragma omp parallel for schedule(static)
  for (int element = 0; element < nElements; ++element) {
    if (element < relb.nelem) {
      seque<int> elementB = relb.lnods[element];
      seque<int> elementA = rela.lnods[element];
      setordered(elementB);
      setordered(elementA);
      relc.lnods[element] = std::move(getdifference(elementA, elementB));
    } else {
      relc.lnods[element] = std::move(rela.lnods[element]);
    }
  }
}
o2m operator-(const o2m &rela, const o2m &relb) {
  o2m relc;
  subtraction(rela, relb, relc);
  return relc;
}
seque<int> toporder(const o2m &rel) {
  seque<int> order;
  setsize(order, 0);
  std::vector<int> inDegree(getsize(rel.lnods), 0);
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for (int i = 0; i < getsize(rel.lnods); ++i) {
    seque<int> const &element = rel.lnods[i];
    for (int node : element) {
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
  while (!q.empty()) {
    int cur = q.front();
    q.pop();
    append(order, cur);
    for (int nbr : rel.lnods[cur]) {
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
                      seque<int> &oldFromNew, seque<int> &newFromOld) {
  indicesfromorder(rel.lnods, elemOrder, oldFromNew, newFromOld);
}
void compresselements(o2m &rel, const seque<int> &oldelementfromnew) {
  rel.lnods = rel.lnods(oldelementfromnew);
  rel.nelem = getsize(oldelementfromnew);
  int local_max = std::transform_reduce(
      std::execution::par, rel.lnods.begin(), rel.lnods.end(), int(0),
      [](int a, int b) { return std::max(a, b); },
      [](seque<int> const &element) {
        return std::reduce(std::execution::par, element.begin(), element.end(),
                           int(0), [](int a, int b) { return std::max(a, b); });
      });
  rel.maxnodenumber = local_max;
}
void permutenodes(o2m &rel, const seque<int> &newnodefromold) {
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for (int i = 0; i < getsize(rel.lnods); ++i) {
    rel.lnods[i] = newnodefromold(rel.lnods[i]);
  }
  int local_max = std::transform_reduce(
      std::execution::par, rel.lnods.begin(), rel.lnods.end(), int(0),
      [](int a, int b) { return std::max(a, b); },
      [](seque<int> const &element) {
        return std::reduce(std::execution::par, element.begin(), element.end(),
                           int(0), [](int a, int b) { return std::max(a, b); });
      });
  rel.maxnodenumber = local_max;
}

#include <omp.h>

// Function to compute the local node index (location) for each node within
// every element.
seque<seque<int>> getlistofnodelocations(o2m const &nodesfromelement,
                                         o2m const &elementsfromnode) {
  // Allocate nodelocation vector with one entry per node.
  seque<seque<int>> nodelocation(elementsfromnode.size());

// Parallelize the per-node allocation.
#pragma omp parallel for
  for (int node = 0; node < elementsfromnode.size(); ++node) {
    setsize(nodelocation[node], elementsfromnode.elementsize(node));
  }

  // Track the next free position for each node.
  seque<int> nodePositionCounter(elementsfromnode.nelem, 0);

// Build the node location lookup table.
// Because multiple elements may update the same node's counter,
// we use an atomic capture to safely increment nodePositionCounter[node].
#pragma omp parallel for
  for (int element = 0; element < nodesfromelement.size(); ++element) {
    const auto &nodes = nodesfromelement.lnods[element];
    int ns = getsize(nodes); // Cache number of nodes in this element.
    for (int localPosition = 0; localPosition < ns; ++localPosition) {
      int node = nodes[localPosition];
      int pos;
// Use atomic capture to safely fetch-and-increment.
#pragma omp atomic capture
      {
        pos = nodePositionCounter[node];
        nodePositionCounter[node]++;
      }
      nodelocation[node][pos] = localPosition;
    }
  }
  return nodelocation;
}

// Function to build the clique addressing table.
o2m getcliqueaddressing(o2m const &nodesfromelement,
                        o2m const &elementsfromnode) {
  // Compute local node positions.
  seque<seque<int>> nodelocations =
      getlistofnodelocations(nodesfromelement, elementsfromnode);

  o2m cliques;
  setnumberofelements(cliques, nodesfromelement.nelem);

// Parallelize allocation of clique matrices for each element.
#pragma omp parallel for
  for (int element = 0; element < nodesfromelement.nelem; ++element) {
    int ns = getsize(nodesfromelement[element]);
    // Replace pow() with simple multiplication for efficiency.
    setsize(cliques[element], ns * ns);
  }

  // Marker array to track if a node was encountered in the current generation.
  seque<int> marker(elementsfromnode.size(), 0);
  // Corrected: nodecounter now sized by the number of nodes.
  seque<int> nodecounter(elementsfromnode.size(), 0);
  int generation = 0;

  // The following nested loops (over nodes, elements, and local nodes)
  // are left sequential due to dependencies on the "generation" and shared
  // arrays.
  for (int node = 0; node < elementsfromnode.size(); ++node) {
    int nnz = 0;
    for (int lelement = 0; lelement < elementsfromnode.elementsize(node);
         ++lelement) {
      generation++; // Increment generation for each element connected to this
                    // node.
      int element = elementsfromnode[node][lelement];
      int nne = nodesfromelement.elementsize(element);
      int localnode = nodelocations[node][lelement];

      // Fill in the clique addressing for each pair (localnode, other local
      // node).
      for (int lnode = 0; lnode < nne; ++lnode) {
        int othernode = nodesfromelement[element][lnode];
        if (marker[othernode] != generation) {
          nodecounter[othernode] = nnz;
          cliques[element][localnode + lnode * nne] = nnz;
          marker[othernode] = generation;
          nnz++;
        } else {
          cliques[element][localnode + lnode * nne] = nodecounter[othernode];
        }
      }
    }
  }
  return cliques;
}
