#include "o2m.hpp"
#include <algorithm>
#include <cassert>
#include <execution>
#include <numeric>
#include <queue>
#include <stdexcept>
#include <vector>
namespace hidden {
size_t update_max_for_nodes(seque<size_t> const &nodes, size_t current_max) {
  return std::reduce(nodes.begin(), nodes.end(), current_max,
                     [](size_t a, size_t b) { return std::max(a, b); });
}
} // namespace hidden
o2m convertfromlist(const seque<size_t> &other) {
  o2m ret;
  setnumberofelements(ret, getsize(other));
  for (size_t element = 0; element < ret.nelem; ++element) {
    setnodesforelement(ret, element, {other[element]});
  }
  return ret;
}
void setnumberofelements(o2m &rel, size_t nelem) {
  rel.nelem = nelem;
  setsize(rel.lnods, nelem);
}
void setnodesforelement(o2m &rel, size_t element, seque<size_t> const &nodes) {
  rel.lnods[element] = nodes;
  rel.maxnodenumber = hidden::update_max_for_nodes(nodes, rel.maxnodenumber);
}

void setnodesforelement(o2m &rel, size_t element, seque<size_t> &&nodes) {
  rel.lnods[element] = std::move(nodes);
  rel.maxnodenumber = hidden::update_max_for_nodes(nodes, rel.maxnodenumber);
}

size_t appendelement(o2m &rel, seque<size_t> const &nodes) {
  rel.nelem++;
  setsize(rel.lnods, rel.nelem);
  assert(getsize(rel.lnods) == rel.nelem);
  rel.lnods[rel.nelem - 1] = nodes;
  rel.maxnodenumber = hidden::update_max_for_nodes(nodes, rel.maxnodenumber);
  return rel.nelem - 1;
}

o2m transpose(const o2m &rel) {
  o2m relt;
  if (rel.nelem == 0) {
    return relt;
  }
  // Here, we assume that the maximum node number in the transposed relation
  // will equal the number of elements from the original relation.
  relt.maxnodenumber = rel.nelem - 1;
  const size_t numberOfNodes = rel.maxnodenumber + 1;
  seque<size_t> counts(numberOfNodes, 0);

  // Parallelize the count of node occurrences per element.
  // Each thread increments counts[nodeId] atomically.
#pragma omp parallel for default(none) shared(rel, counts)
  for (size_t elementIdx = 0; elementIdx < rel.nelem; ++elementIdx) {
    for (size_t nodeId : rel.lnods[elementIdx]) {
#pragma omp atomic
      counts[nodeId]++;
    }
  }

  // Set up the transposed structure with the proper number of "nodes".
  setnumberofelements(relt, numberOfNodes);

  // Parallelize the allocation of each node's list.
  // Each iteration is independent.
#pragma omp parallel for default(none) shared(relt, counts, numberOfNodes)
  for (size_t node = 0; node < numberOfNodes; ++node) {
    setsize(relt.lnods[node], counts[node]);
  }

  seque<size_t> offsets(numberOfNodes, 0);

  // Parallelize filling the transposed data structure.
  // For each element in the original relation, write its index into the
  // appropriate node's list. Atomic capture guarantees that each update of
  // offsets[node] is unique.
#pragma omp parallel for default(none) shared(rel, offsets, relt)
  for (size_t elementIndex = 0; elementIndex < rel.nelem; ++elementIndex) {
    for (size_t node : rel.lnods[elementIndex]) {
      size_t pos;
#pragma omp atomic capture
      {
        pos = offsets[node];
        offsets[node]++;
      }
      relt.lnods[node][pos] = elementIndex;
    }
  }
  return relt;
}

void multiplication(const o2m &rel, const seque<size_t> &vec, o2m &vecresult) {
  o2m rel2 = convertfromlist(vec);
  multiplication(rel, rel2, vecresult);
}

#include <cstring> // For std::memcpy

void multiplication54(const o2m &rela, const o2m &relb, o2m &relc) {
  // Initialize the output container "relc" based on the input "rela".
  setnumberofelements(relc, rela.nelem);
  if (rela.nelem == 0 || relb.maxnodenumber == 0)
    return;
  relc.maxnodenumber = relb.maxnodenumber;

  // Begin OpenMP parallel region.
#pragma omp parallel
  {
    // Allocate a thread-local generational marker array.
    // Each index corresponds to a node, storing the generation when it was last
    // marked.
    seque<size_t> marker(relb.maxnodenumber + 1, 0);

    // Allocate a temporary container to record unique node IDs in pass one.
    // Preallocated to the maximum possible capacity.
    seque<size_t> touchedNodes(relb.maxnodenumber + 1, 0);
    size_t ntouched = 0; // Counter for unique nodes.

    // A thread-local counter to generate a unique marker for each iteration.
    size_t localGeneration = 1;

    // Distribute iterations over the elements in 'rela' among threads.
#pragma omp for schedule(static)
    for (size_t elementAIndex = 0; elementAIndex < rela.nelem;
         ++elementAIndex) {
      // Grab the input element for this iteration.
      const seque<size_t> &elementA = rela.lnods[elementAIndex];

      // Assign a unique generation value for the current iteration.
      size_t currentGeneration = localGeneration++;

      // Reset the unique nodes counter.
      ntouched = 0;

      // --- Pass One: Marking and Recording Unique Node IDs ---
      for (size_t nodeA : elementA) {
        // Most nodeA values are expected to be valid.
        if (__builtin_expect(nodeA >= relb.nelem, 0))
          continue; // Skip invalid indices.

        // Get the corresponding elementB from relb.
        const seque<size_t> &elementB = relb.lnods[nodeA];
        const size_t bSize = getsize(elementB);

        // Iterate over elementB.
        for (size_t j = 0; j < bSize; ++j) {
#if defined(__GNUC__) || defined(__clang__)
          // Prefetch the next element to reduce memory latency.
          if (j + 1 < bSize)
            __builtin_prefetch(&elementB[j + 1], 0, 3);
#endif
          size_t nodeB = elementB[j];
          // Hint that the condition is most likely true.
          if (__builtin_expect(marker[nodeB] != currentGeneration, 1)) {
            // Mark nodeB with the current generation.
            marker[nodeB] = currentGeneration;
            // Record the unique node.
            touchedNodes[ntouched++] = nodeB;
          }
        }
      } // end Pass One

      // --- Pass Two: Building the Final Output ---
      // Resize relc for the current element to the exact count of unique nodes.
      setsize(relc[elementAIndex], ntouched);

      // Use memcpy if there is something to copy.
      if (ntouched > 0) {
        // Assuming seque's underlying storage is contiguous.
        std::memcpy(relc[elementAIndex].begin(), touchedNodes.actual,
                    ntouched * sizeof(size_t));
      }
    } // end for each elementA
  } // end parallel region
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
    seque<size_t> touchedNodes;

    // A container for the final result. We’ll allocate it exactly
    // to the number of unique nodes found.
    seque<size_t> resultNodes;

#pragma omp for schedule(static)
    for (size_t elementAIndex = 0; elementAIndex < rela.nelem;
         ++elementAIndex) {
      const seque<size_t> &elementA = rela.lnods[elementAIndex];

      // --- Pass One: Mark Unique Nodes and Record Their IDs ---
      // Clear any remnants from prior iterations.
      erase(touchedNodes);

      for (size_t nodeA : elementA) {
        // Only consider valid indices.
        if (nodeA >= relb.nelem)
          continue;
        const seque<size_t> &elementB = relb.lnods[nodeA];
        size_t bSize = getsize(elementB);

        for (size_t j = 0; j < bSize; ++j) {
#if defined(__GNUC__) || defined(__clang__)
          if (j + 1 < bSize)
            __builtin_prefetch(&elementB[j + 1], 0, 3);
#endif
          size_t nodeB = elementB[j];
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
      for (size_t i = 0; i < touchedNodes.size; ++i) {
        resultNodes[i] = touchedNodes[i];
      }

      // --- Cleanup: Reset the visited Flags ---
      // Instead of scanning the entire "visited" array, we only reset the nodes
      // touched.
      for (size_t nodeB : touchedNodes) {
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

o2m operator*(const o2m &rela, const seque<size_t> &vec) {
  o2m relc;
  multiplication(rela, vec, relc);
  return relc;
}

void addition(const o2m &rela, const o2m &relb, o2m &relc) {
  size_t maxElements = std::max(rela.nelem, relb.nelem);
  setnumberofelements(relc, maxElements);
  relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
#pragma omp parallel
  {
    std::vector<size_t> marker(relc.maxnodenumber + 1, 0);
    size_t local_generation = 1;
#pragma omp for schedule(static)
    for (size_t element = 0; element < maxElements; ++element) {
      size_t count = 0;
      if (element < rela.nelem) {
        const seque<size_t> &elementA = rela.lnods[element];
        for (size_t node : elementA) {
          if (marker[node] != local_generation) {
            marker[node] = local_generation;
            count++;
          }
        }
      }
      if (element < relb.nelem) {
        const seque<size_t> &elementB = relb.lnods[element];
        for (size_t node : elementB) {
          if (marker[node] != local_generation) {
            marker[node] = local_generation;
            count++;
          }
        }
      }
      seque<size_t> temp(count);
      count = 0;
      if (element < rela.nelem) {
        const seque<size_t> &elementA = rela.lnods[element];
        for (size_t node : elementA) {
          if (marker[node] == local_generation) {
            temp[count++] = node;
            marker[node] = local_generation + 1;
          }
        }
      }
      if (element < relb.nelem) {
        const seque<size_t> &elementB = relb.lnods[element];
        for (size_t node : elementB) {
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
  const size_t nElements = std::min(rela.nelem, relb.nelem);
  setnumberofelements(relc, nElements);
  relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
#pragma omp parallel for schedule(static)
  for (size_t element = 0; element < nElements; ++element) {
    seque<size_t> elementB = relb.lnods[element];
    seque<size_t> elementA = rela.lnods[element];
    setordered(elementB);
    setordered(elementA);
    relc.lnods[element] = std::move(getintersection(elementA, elementB));
  }
}
void subtraction(const o2m &rela, const o2m &relb, o2m &relc) {
  const size_t nElements = rela.nelem;
  setnumberofelements(relc, nElements);
  relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
#pragma omp parallel for schedule(static)
  for (size_t element = 0; element < nElements; ++element) {
    if (element < relb.nelem) {
      seque<size_t> elementB = relb.lnods[element];
      seque<size_t> elementA = rela.lnods[element];
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
seque<size_t> toporder(const o2m &rel) {
  seque<size_t> order;
  setsize(order, 0);
  std::vector<size_t> inDegree(getsize(rel.lnods), 0);
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for (size_t i = 0; i < getsize(rel.lnods); ++i) {
    seque<size_t> const &element = rel.lnods[i];
    for (size_t node : element) {
#ifdef _OPENMP
#pragma omp atomic
#endif
      ++inDegree[node];
    }
  }
  std::queue<size_t> q;
  for (size_t i = 0; i < inDegree.size(); ++i)
    if (inDegree[i] == 0)
      q.push(i);
  while (!q.empty()) {
    size_t cur = q.front();
    q.pop();
    append(order, cur);
    for (size_t nbr : rel.lnods[cur]) {
      if (--inDegree[nbr] == 0)
        q.push(nbr);
    }
  }
  if (getsize(order) != getsize(rel.lnods))
    throw std::runtime_error(
        "The relation contains cycles, topological sort not possible.");
  return order;
}
seque<size_t> lexiorder(const o2m &rel) { return getorder(rel.lnods); }
void indicesfromorder(const o2m &rel, const seque<size_t> &elemOrder,
                      seque<size_t> &oldFromNew, seque<size_t> &newFromOld) {
  indicesfromorder(rel.lnods, elemOrder, oldFromNew, newFromOld);
}
void compresselements(o2m &rel, const seque<size_t> &oldelementfromnew) {
  rel.lnods = rel.lnods(oldelementfromnew);
  rel.nelem = getsize(oldelementfromnew);
  size_t local_max = std::transform_reduce(
      std::execution::par, rel.lnods.begin(), rel.lnods.end(), size_t(0),
      [](size_t a, size_t b) { return std::max(a, b); },
      [](seque<size_t> const &element) {
        return std::reduce(std::execution::par, element.begin(), element.end(),
                           size_t(0),
                           [](size_t a, size_t b) { return std::max(a, b); });
      });
  rel.maxnodenumber = local_max;
}
void permutenodes(o2m &rel, const seque<size_t> &newnodefromold) {
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for (size_t i = 0; i < getsize(rel.lnods); ++i) {
    rel.lnods[i] = newnodefromold(rel.lnods[i]);
  }
  size_t local_max = std::transform_reduce(
      std::execution::par, rel.lnods.begin(), rel.lnods.end(), size_t(0),
      [](size_t a, size_t b) { return std::max(a, b); },
      [](seque<size_t> const &element) {
        return std::reduce(std::execution::par, element.begin(), element.end(),
                           size_t(0),
                           [](size_t a, size_t b) { return std::max(a, b); });
      });
  rel.maxnodenumber = local_max;
}
seque<seque<size_t>> getnodelocation(o2m const &nodesfromelement,
                                     o2m const &elementsfromnode) {
  seque<seque<size_t>> nodelocation(elementsfromnode.size());
  for (size_t node = 0; node < elementsfromnode.size(); ++node) {
    setsize(nodelocation[node], elementsfromnode.elementsize(node));
  }
  // Track the next position to fill for each node
  seque<size_t> nodePositionCounter(elementsfromnode.nelem, 0);
  // Build the node location lookup table
  for (size_t element = 0; element < nodesfromelement.size(); ++element) {
    const auto &nodes = nodesfromelement.lnods[element];
    for (size_t localPosition = 0; localPosition < getsize(nodes);
         ++localPosition) {
      size_t node = nodes[localPosition];
      nodelocation[node][nodePositionCounter[node]++] = localPosition;
    }
  }
  return nodelocation;
}
