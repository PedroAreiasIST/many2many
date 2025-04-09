#include "o2m.hpp"
#include <algorithm>
#include <cassert>
#include <execution>
#include <numeric> // for std::fill (if needed)
#include <omp.h>
#include <queue>
#include <stdexcept>
#include <vector>
namespace hidden {

// Returns the maximum value found in 'nodes', starting from current_max.
size_t update_max_for_nodes(seque<size_t> const &nodes, size_t current_max) {
  // Use parallel reduction with std::execution::par.
  return std::reduce(std::execution::par, nodes.begin(), nodes.end(),
                     current_max,
                     [](size_t a, size_t b) { return std::max(a, b); });
}

// Sets the number of elements (domain size) in the relation.
void setnumberofelements(o2m &rel, size_t nelem) {
  rel.nelem = nelem;
  setsize(rel.lnods, nelem);
}

// Sets the list of nodes for a given element, updating maxnodenumber.
void setnodesforelement(o2m &rel, size_t element, seque<size_t> const &nodes) {
  rel.lnods[element] = nodes;
  // Update maxnodenumber using a parallel reduction.
  for (size_t i = 0; i < getsize(nodes); ++i)
    rel.maxnodenumber = std::max(rel.maxnodenumber, nodes[i]);
}

// Appends an element (its node list) to the relation and updates maxnodenumber.
size_t appendelement(o2m &rel, seque<size_t> const &nodes) {
  rel.nelem++;
  setsize(rel.lnods, rel.nelem);
  assert(getsize(rel.lnods) == rel.nelem);
  rel.lnods[rel.nelem - 1] = nodes;
  // Update maxnodenumber using a parallel reduction.
  rel.maxnodenumber = std::reduce(
      std::execution::par, nodes.begin(), nodes.end(), rel.maxnodenumber,
      [](size_t a, size_t b) { return std::max(a, b); });
  return rel.nelem - 1;
}

void transpose(const o2m &rel, o2m &relt) {
  // Initialize output relation metadata and clear node lists.
  relt.maxnodenumber = 0;
  relt.nelem = 0;
  erase(relt.lnods);
  // Early return on empty input.
  if (rel.nelem == 0) {
    return;
  }
  // Number of nodes is one more than the maximum node number.
  const size_t numberOfNodes = rel.maxnodenumber + 1;

  // *** First Pass: Count Occurrences per Node ***
  // Create a global counts array with one entry per node.
  std::vector<size_t> counts(numberOfNodes, 0);

  // Parallelize the loop over elements.
  for (size_t elementIndex = 0; elementIndex < rel.nelem; ++elementIndex) {
    for (size_t node : rel.lnods[elementIndex]) {
      counts[node]++;
    }
  }

  // *** Preallocation: Prepare the Output Relation ***
  setnumberofelements(relt, numberOfNodes);
  for (size_t node = 0; node < numberOfNodes; ++node) {
    // Preallocate exactly the number of elements counted for each node.
    setsize(relt.lnods[node], counts[node]);
  }

  // *** Second Pass: Fill the Transposed Relation ***
  // Prepare a write offsets array to track where to write for each node.
  std::vector<size_t> offsets(numberOfNodes, 0);

  // Parallelize the loop over elements.
  for (size_t elementIndex = 0; elementIndex < rel.nelem; ++elementIndex) {
    for (size_t node : rel.lnods[elementIndex]) {
      size_t pos;
      pos = offsets[node]++;
      relt.lnods[node][pos] = elementIndex;
    }
  }
}

// Assumed helper functions and types:
// - erase(x): clears container x.
// - setnumberofelements(rel, n): sets rel to have n elements.
// - setsize(x, n): resizes container x to exactly n elements.
// - o2m is a structure holding:
//      size_t nelem;               // number of elements (rows)
//      size_t maxnodenumber;       // maximum node index (used for marker
//      arrays) seque<size_t> lnods[];      // container of node lists (each
//      seque acts like a vector)
// - seque<size_t> is assumed to support clear(), resize(), reserve(),
// capacity(), operator[], size().

void multiplication(const o2m &rela, const o2m &relb, o2m &relc) {
  // Initialize the output relation to have one element per rela element.
  setnumberofelements(relc, rela.nelem);
  relc.maxnodenumber = relb.maxnodenumber;

  // Early exit if either relation is empty.
  if (rela.nelem == 0 || relb.maxnodenumber == 0) {
    return;
  }

#pragma omp parallel
  {
    // Allocate a thread-local marker array (one entry per possible node).
    seque<size_t> marker(relb.maxnodenumber + 1, 0);
    // Temporary container for collecting result nodes.
    seque<size_t> resultNodes;
    // Generation counter used for marking (incremented by 2 each element).
    size_t currentGeneration = 1;

// Process each element (row) in rela with static scheduling.
#pragma omp for schedule(static)
    for (size_t elementAIndex = 0; elementAIndex < rela.nelem;
         ++elementAIndex) {
      const seque<size_t> &elementA = rela.lnods[elementAIndex];
      size_t uniqueCount = 0;

      // --- Phase 1: Mark unique reachable nodes ---
      // Precompute an upper bound on needed capacity.
      size_t estCapacity = 0;
      for (size_t nodeA : elementA) {
        if (nodeA < relb.nelem) {
          estCapacity += getsize(relb.lnods[nodeA]);
        }
      }
      setsize(resultNodes, estCapacity);

      // Walk over node lists from rela's element.
      for (size_t i = 0, aSize = getsize(elementA); i < aSize; ++i) {
        size_t nodeA = elementA[i];
        if (nodeA >= relb.nelem) {
          continue;
        }
        const seque<size_t> &elementB = relb.lnods[nodeA];
        for (size_t j = 0, bSize = getsize(elementB); j < bSize; ++j) {
#if defined(__GNUC__) || defined(__clang__)
          // Prefetch next entry in elementB.
          if (j + 1 < bSize) {
            __builtin_prefetch(&elementB[j + 1], 0, 3);
          }
#endif
          size_t nodeB = elementB[j];
          // Mark nodeB only if it hasn't been seen in the current generation.
          if (marker[nodeB] != currentGeneration) {
            marker[nodeB] = currentGeneration;
            ++uniqueCount;
          }
        }
      }

      // Resize resultNodes to fit the unique nodes we expect.
      // (The size will be overwritten in Phase 2.)
      setsize(resultNodes, uniqueCount);
      uniqueCount = 0; // reuse as insertion index

      // --- Phase 2: Collect the marked nodes ---
      for (size_t i = 0, aSize = getsize(elementA); i < aSize; ++i) {
        size_t nodeA = elementA[i];
        if (nodeA >= relb.nelem) {
          continue;
        }
        const seque<size_t> &elementB = relb.lnods[nodeA];
        for (size_t j = 0, bSize = getsize(elementB); j < bSize; ++j) {
#if defined(__GNUC__) || defined(__clang__)
          if (j + 1 < bSize) {
            __builtin_prefetch(&elementB[j + 1], 0, 3);
          }
#endif
          size_t nodeB = elementB[j];
          // Only collect nodes still marked with currentGeneration.
          if (marker[nodeB] == currentGeneration) {
            resultNodes[uniqueCount++] = nodeB;
            // Update the marker to avoid collecting duplicates.
            marker[nodeB] = currentGeneration + 1;
          }
        }
      }

      // Save the computed result into the output relation.
      relc.lnods[elementAIndex] = resultNodes;
      // Clear the local result vector (without releasing reserved memory).
      setsize(resultNodes, 0);

      // Increment generation counter by 2 (to separate marking and collection
      // phases).
      currentGeneration += 2;
    }
  }
}

// Adds (unites) two relations elementwise.
// For each element, the union of the node lists is computed.
// (Note: duplicates are removed using a marker technique, but ordering is
// arbitrary.)
void addition(const o2m &rela, const o2m &relb, o2m &relc) {
  size_t maxElements = std::max(rela.nelem, relb.nelem);
  setnumberofelements(relc, maxElements);
  relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
#ifdef _OPENMP
#pragma omp parallel
  {
    // Allocate a thread-local marker array once per thread.
    seque<size_t> marker(relc.maxnodenumber + 1, 0);
#pragma omp for schedule(dynamic)
    for (size_t element = 0; element < maxElements; ++element) {
      // Reinitialize marker for this iteration.
      std::fill(marker.begin(), marker.end(), 0);
      size_t currentGeneration = 1;
      size_t count = 0;
      if (element < rela.nelem) {
        seque<size_t> const &elementA = rela.lnods[element];
        for (size_t node : elementA) {
          if (marker[node] != currentGeneration) {
            marker[node] = currentGeneration;
            count++;
          }
        }
      }
      if (element < relb.nelem) {
        seque<size_t> const &elementB = relb.lnods[element];
        for (size_t node : elementB) {
          if (marker[node] != currentGeneration) {
            marker[node] = currentGeneration;
            count++;
          }
        }
      }
      seque<size_t> temp;
      setsize(temp, count);
      count = 0;
      if (element < rela.nelem) {
        seque<size_t> const &elementA = rela.lnods[element];
        for (size_t node : elementA) {
          if (marker[node] == currentGeneration) {
            temp[count++] = node;
            marker[node] = currentGeneration + 1;
          }
        }
      }
      if (element < relb.nelem) {
        seque<size_t> const &elementB = relb.lnods[element];
        for (size_t node : elementB) {
          if (marker[node] == currentGeneration) {
            temp[count++] = node;
            marker[node] = currentGeneration + 1;
          }
        }
      }
      relc.lnods[element] = temp;
      // currentGeneration update is local per iteration.
    }
  }
#else
  // Fallback serial implementation (unchanged).
  for (size_t element = 0; element < maxElements; ++element) {
    seque<size_t> marker(relc.maxnodenumber + 1, 0);
    size_t currentGeneration = 1;
    size_t count = 0;
    if (element < rela.nelem) {
      seque<size_t> const &elementA = rela.lnods[element];
      for (size_t node : elementA) {
        if (marker[node] != currentGeneration) {
          marker[node] = currentGeneration;
          count++;
        }
      }
    }
    if (element < relb.nelem) {
      seque<size_t> const &elementB = relb.lnods[element];
      for (size_t node : elementB) {
        if (marker[node] != currentGeneration) {
          marker[node] = currentGeneration;
          count++;
        }
      }
    }
    seque<size_t> temp;
    setsize(temp, count);
    count = 0;
    if (element < rela.nelem) {
      seque<size_t> const &elementA = rela.lnods[element];
      for (size_t node : elementA) {
        if (marker[node] == currentGeneration) {
          temp[count++] = node;
          marker[node] = currentGeneration + 1;
        }
      }
    }
    if (element < relb.nelem) {
      seque<size_t> const &elementB = relb.lnods[element];
      for (size_t node : elementB) {
        if (marker[node] == currentGeneration) {
          temp[count++] = node;
          marker[node] = currentGeneration + 1;
        }
      }
    }
    relc.lnods[element] = temp;
  }
#endif
}

// Computes the intersection of two relations elementwise.
// The domain of the result is set to the minimum of the two inputs.
void intersection(const o2m &a, const o2m &b, o2m &c) {
  const size_t nElements = std::min(a.nelem, b.nelem);
  setnumberofelements(c, nElements);
  c.maxnodenumber = std::max(a.maxnodenumber, b.maxnodenumber);
#ifdef _OPENMP
#pragma omp parallel
  {
    // Allocate a thread-local marker array once per thread.
    seque<size_t> marker(c.maxnodenumber + 1);
#pragma omp for schedule(dynamic)
    for (size_t element = 0; element < nElements; ++element) {
      std::fill(marker.begin(), marker.end(), 0);
      size_t currentGeneration = 1;
      seque<size_t> common;
      const seque<size_t> &elementB = b.lnods[element];
      for (size_t node : elementB) {
        marker[node] = currentGeneration;
      }
      size_t count = 0;
      const seque<size_t> &elementA = a.lnods[element];
      for (size_t node : elementA) {
        if (marker[node] == currentGeneration)
          count++;
      }
      setsize(common, count);
      count = 0;
      for (size_t node : elementA) {
        if (marker[node] == currentGeneration) {
          common[count++] = node;
          marker[node] = currentGeneration + 1;
        }
      }
      c.lnods[element] = common;
    }
  }
#else
  // Fallback serial implementation.
  for (size_t element = 0; element < nElements; ++element) {
    seque<size_t> marker(c.maxnodenumber + 1);
    seque<size_t> common;
    size_t currentGeneration = 1;
    const seque<size_t> &elementB = b.lnods[element];
    for (size_t node : elementB) {
      marker[node] = currentGeneration;
    }
    size_t count = 0;
    const seque<size_t> &elementA = a.lnods[element];
    for (size_t node : elementA) {
      if (marker[node] == currentGeneration)
        count++;
    }
    setsize(common, count);
    count = 0;
    for (size_t node : elementA) {
      if (marker[node] == currentGeneration) {
        common[count++] = node;
        marker[node] = currentGeneration + 1;
      }
    }
    c.lnods[element] = common;
  }
#endif
}

// Computes the difference of two relations (rela - relb) elementwise.
void subtraction(const o2m &rela, const o2m &relb, o2m &relc) {
  const size_t nElements = rela.nelem;
  setnumberofelements(relc, nElements);
  relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
#ifdef _OPENMP
#pragma omp parallel
  {
    // Allocate a thread-local marker array once per thread.
    seque<size_t> marker(relc.maxnodenumber + 1, 0);
#pragma omp for schedule(dynamic)
    for (size_t element = 0; element < nElements; ++element) {
      std::fill(marker.begin(), marker.end(), 0);
      if (element < relb.nelem) {
        const seque<size_t> &elementB = relb.lnods[element];
        for (size_t node : elementB)
          marker[node] = 1;
      }
      const seque<size_t> &elementA = rela.lnods[element];
      size_t count = 0;
      for (size_t node : elementA)
        if (marker[node] == 0)
          count++;
      seque<size_t> diff;
      setsize(diff, count);
      count = 0;
      for (size_t node : elementA)
        if (marker[node] == 0)
          diff[count++] = node;
      relc.lnods[element] = diff;
    }
  }
#else
  // Fallback serial implementation.
  for (size_t element = 0; element < nElements; ++element) {
    seque<size_t> marker(relc.maxnodenumber + 1, 0);
    if (element < relb.nelem) {
      const seque<size_t> &elementB = relb.lnods[element];
      for (size_t node : elementB)
        marker[node] = 1;
    }
    const seque<size_t> &elementA = rela.lnods[element];
    size_t count = 0;
    for (size_t node : elementA)
      if (marker[node] == 0)
        count++;
    seque<size_t> diff;
    setsize(diff, count);
    count = 0;
    for (size_t node : elementA)
      if (marker[node] == 0)
        diff[count++] = node;
    relc.lnods[element] = diff;
  }
#endif
}

// Computes a topological order of the graph represented by 'rel'.
// Assumes 'rel.lnods' is sized to cover all vertices. Throws a runtime_error
// if a cycle exists.
seque<size_t> toporder(const o2m &rel) {
  seque<size_t> order;
  setsize(order, 0);
  // 'inDegree' is sized to the number of vertices.
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

// Returns a lexicographical order of the relation's elements.
seque<size_t> lexiorder(const o2m &rel) { return getorder(rel.lnods); }

// Generates index mappings based on a given element order.
void indicesfromorder(const o2m &rel, const seque<size_t> &elemOrder,
                      seque<size_t> &oldFromNew, seque<size_t> &newFromOld) {
  indicesfromorder(rel.lnods, elemOrder, oldFromNew, newFromOld);
}

// Compresses the elements of the relation based on the provided mapping.
void compresselements(o2m &rel, const seque<size_t> &oldelementfromnew) {
  rel.lnods = rel.lnods(oldelementfromnew);
  rel.nelem = getsize(oldelementfromnew);
  // Use a parallel reduction over elements to compute the overall maximum.
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

// Permutes the node indices of the relation based on the newnodefromold
// mapping.
void permutenodes(o2m &rel, const seque<size_t> &newnodefromold) {
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for (size_t i = 0; i < getsize(rel.lnods); ++i) {
    rel.lnods[i] = newnodefromold(rel.lnods[i]);
  }
  // Update maxnodenumber using a parallel reduction.
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

} // namespace hidden
