#include "o2m.hpp"
#include <algorithm>
#include <cassert>
#include <execution>
#include <queue>
#include <stdexcept>
#include <unordered_set>
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
  rel.maxnodenumber = std::reduce(
      std::execution::par, nodes.begin(), nodes.end(), rel.maxnodenumber,
      [](size_t a, size_t b) { return std::max(a, b); });
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

// Transposes the one-to-many relation. The result will have as its domain all
// nodes (assumed to be numbered 0 .. rel.maxnodenumber) and for each node, the
// list of elements that point to it. (Note: duplicate entries are not removed.)
void transpose(const o2m &rel, o2m &relt) {
  relt.maxnodenumber = 0;
  relt.nelem = 0;
  erase(relt.lnods);
  const size_t numberofnodes = rel.maxnodenumber + 1;
  seque<size_t> counts(numberofnodes, 0);
  // Count how many times each node is referenced.
  for (size_t localelement = 0; localelement < rel.nelem; ++localelement) {
    seque<size_t> const &temp = rel.lnods[localelement];
    for (size_t node : temp) {
      counts[node]++;
    }
  }
  // Set the domain of the transposed relation to include all nodes.
  setnumberofelements(relt, numberofnodes);
  for (size_t localnode = 0; localnode < numberofnodes; ++localnode) {
    setsize(relt.lnods[localnode], counts[localnode]);
  }
  // Temporary marker array to manage positions.
  seque<size_t> genmarker(numberofnodes, 0);
  size_t currentgeneration = 1;
  // Process each element sequentially to fill in the transposed lists.
  for (size_t element = 0; element < rel.nelem; ++element) {
    seque<size_t> const &temp = rel.lnods[element];
    for (size_t node : temp) {
      if (genmarker[node] != currentgeneration) {
        counts[node] = 0;
        genmarker[node] = currentgeneration;
      }
      relt.lnods[node][counts[node]++] = element;
      // Update maxnodenumber of the transposed relation.
      relt.maxnodenumber = std::max(relt.maxnodenumber, element);
    }
    currentgeneration++;
  }
}

void multiplication(const o2m &rela, const o2m &relb, o2m &relc) {
  // The resulting relation's domain is rela's.
  setnumberofelements(relc, rela.nelem);
  // Assume that rela's node entries refer to valid indices in relb.
  relc.maxnodenumber = relb.maxnodenumber;

  // Process each row of rela in parallel.
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for (size_t aRow = 0; aRow < rela.nelem; ++aRow) {
    std::unordered_set<size_t> unique_nodes;
    seque<size_t> const &rowA = rela.lnods[aRow];

    // For each node in the current row of rela, traverse relb.
    for (size_t aCol : rowA) {
      if (aCol < relb.nelem) { // Ensure aCol is valid.
        seque<size_t> const &rowB = relb.lnods[aCol];
        for (size_t bCol : rowB) {
          unique_nodes.insert(bCol);
        }
      }
    }

    // Create a temporary seque from the unique nodes.
    seque<size_t> temp;
    for (auto node : unique_nodes) {
      append(temp, node);
    }

    // Assign the computed row to the result relation.
    relc.lnods[aRow] = temp;
  }
}

// Adds (unites) two relations elementwise.
// For each element (row), the union of the node lists is computed.
// (Note: duplicates are removed using a marker technique, but ordering is
// arbitrary.)
void addition(const o2m &rela, const o2m &relb, o2m &relc) {
  size_t maxelem = std::max(rela.nelem, relb.nelem);
  setnumberofelements(relc, maxelem);
  relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
  // Process each row in parallel.
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for (size_t row = 0; row < maxelem; ++row) {
    // Each thread uses its own marker and temporary container.
    seque<size_t> marker(relc.maxnodenumber + 1, 0);
    size_t currentGeneration = 1;
    size_t count = 0;
    if (row < rela.nelem) {
      seque<size_t> const &rowA = rela.lnods[row];
      for (size_t node : rowA) {
        if (marker[node] != currentGeneration) {
          marker[node] = currentGeneration;
          count++;
        }
      }
    }
    if (row < relb.nelem) {
      seque<size_t> const &rowB = relb.lnods[row];
      for (size_t node : rowB) {
        if (marker[node] != currentGeneration) {
          marker[node] = currentGeneration;
          count++;
        }
      }
    }
    seque<size_t> temp;
    setsize(temp, count);
    count = 0;
    if (row < rela.nelem) {
      seque<size_t> const &rowA = rela.lnods[row];
      for (size_t node : rowA) {
        if (marker[node] == currentGeneration) {
          temp[count++] = node;
          marker[node] = currentGeneration + 1;
        }
      }
    }
    if (row < relb.nelem) {
      seque<size_t> const &rowB = relb.lnods[row];
      for (size_t node : rowB) {
        if (marker[node] == currentGeneration) {
          temp[count++] = node;
          marker[node] = currentGeneration + 1;
        }
      }
    }
    relc.lnods[row] = temp;
    // currentGeneration is local per row.
    currentGeneration += 2;
  }
}

// Computes the intersection of two relations elementwise.
// The domain of the result is set to the minimum of the two inputs.
void intersection(const o2m &a, const o2m &b, o2m &c) {
  const size_t nRows = std::min(a.nelem, b.nelem);
  setnumberofelements(c, nRows);
  c.maxnodenumber = std::max(a.maxnodenumber, b.maxnodenumber);
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for (size_t r = 0; r < nRows; ++r) {
    seque<size_t> marker(c.maxnodenumber + 1);
    seque<size_t> common;
    size_t currentGeneration = 1;
    const seque<size_t> &rowB = b.lnods[r];
    for (size_t node : rowB) {
      marker[node] = currentGeneration;
    }
    size_t count = 0;
    const seque<size_t> &rowA = a.lnods[r];
    for (size_t node : rowA) {
      if (marker[node] == currentGeneration)
        count++;
    }
    setsize(common, count);
    count = 0;
    for (size_t node : rowA) {
      if (marker[node] == currentGeneration) {
        common[count++] = node;
        marker[node] = currentGeneration + 1;
      }
    }
    c.lnods[r] = common;
    currentGeneration += 2;
  }
}

// Computes the difference of two relations (rela - relb) elementwise.
void subtraction(const o2m &rela, const o2m &relb, o2m &relc) {
  const size_t nRows = rela.nelem;
  setnumberofelements(relc, nRows);
  relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for (size_t r = 0; r < nRows; ++r) {
    seque<size_t> marker(relc.maxnodenumber + 1, 0);
    if (r < relb.nelem) {
      const seque<size_t> &rowB = relb.lnods[r];
      for (size_t node : rowB)
        marker[node] = 1;
    }
    const seque<size_t> &rowA = rela.lnods[r];
    size_t count = 0;
    for (size_t node : rowA)
      if (marker[node] == 0)
        count++;
    seque<size_t> diff;
    setsize(diff, count);
    count = 0;
    for (size_t node : rowA)
      if (marker[node] == 0)
        diff[count++] = node;
    relc.lnods[r] = diff;
  }
}

// Computes a topological order of the graph represented by 'rel'.
// Assumes 'rel.lnods' is sized to cover all vertices. Throws a runtime_error if
// a cycle exists.
seque<size_t> toporder(const o2m &rel) {
  seque<size_t> order;
  setsize(order, 0);
  // 'inDegree' is sized to the number of vertices.
  std::vector<size_t> inDegree(getsize(rel.lnods), 0);
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for (size_t i = 0; i < getsize(rel.lnods); ++i) {
    seque<size_t> const &row = rel.lnods[i];
    for (size_t node : row) {
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

// Returns a lexicographical order of the relation's rows.
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
  // Use a parallel reduction over rows to compute the overall maximum.
  size_t local_max = std::transform_reduce(
      std::execution::par, rel.lnods.begin(), rel.lnods.end(), size_t(0),
      [](size_t a, size_t b) { return std::max(a, b); },
      [](seque<size_t> const &row) {
        return std::reduce(std::execution::par, row.begin(), row.end(),
                           size_t(0),
                           [](size_t a, size_t b) { return std::max(a, b); });
      });
  rel.maxnodenumber = local_max;
}

// Permutes the node indices of the relation based on newnodefromold mapping.
void permutenodes(o2m &rel, const seque<size_t> &newnodefromold) {
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for (size_t i = 0; i < getsize(rel.lnods); ++i) {
    rel.lnods[i] = newnodefromold(rel.lnods[i]);
  }
  // Update maxnodenumber using parallel reduction.
  size_t local_max = std::transform_reduce(
      std::execution::par, rel.lnods.begin(), rel.lnods.end(), size_t(0),
      [](size_t a, size_t b) { return std::max(a, b); },
      [](seque<size_t> const &row) {
        return std::reduce(std::execution::par, row.begin(), row.end(),
                           size_t(0),
                           [](size_t a, size_t b) { return std::max(a, b); });
      });
  rel.maxnodenumber = local_max;
}

} // namespace hidden
