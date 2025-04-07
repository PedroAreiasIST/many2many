#include "o2m.hpp"
#include <algorithm>
#include <cassert>
#include <queue>
#include <stdexcept>
#include <vector>

namespace hidden {

size_t update_max_for_nodes(seque<size_t> const &nodes, size_t current_max) {
  size_t max_val = current_max;
#ifdef _OPENMP
#pragma omp parallel for reduction(max:max_val)
#endif
  for (size_t i = 0; i < getsize(nodes); ++i) {
    max_val = std::max(max_val, nodes[i]);
  }
  return max_val;
}

void setnumberofelements(o2m &rel, size_t nelem) {
  rel.nelem = nelem;
  setsize(rel.lnods, nelem);
}

void setnodesforelement(o2m &rel, size_t element, seque<size_t> const &nodes) {
  rel.lnods[element] = nodes;
  size_t local_max = rel.maxnodenumber;
#ifdef _OPENMP
#pragma omp parallel for reduction(max:local_max)
#endif
  for (size_t i = 0; i < getsize(nodes); ++i) {
    local_max = std::max(local_max, nodes[i]);
  }
  rel.maxnodenumber = local_max;
}

size_t appendelement(o2m &rel, seque<size_t> const &nodes) {
  rel.nelem++;
  setsize(rel.lnods, rel.nelem);
  assert(getsize(rel.lnods) == rel.nelem);
  rel.lnods[rel.nelem - 1] = nodes;
  size_t local_max = rel.maxnodenumber;
#ifdef _OPENMP
#pragma omp parallel for reduction(max:local_max)
#endif
  for (size_t i = 0; i < getsize(nodes); ++i) {
    local_max = std::max(local_max, nodes[i]);
  }
  rel.maxnodenumber = local_max;
  return rel.nelem - 1;
}

void transpose(const o2m &rel, o2m &relt) {
  relt.maxnodenumber = 0;
  relt.nelem = 0;
  erase(relt.lnods);
  const size_t numberofnodes = rel.maxnodenumber + 1;
  seque<size_t> counts(numberofnodes, 0);
  // Update counts concurrently with atomic increments.
#ifdef _OPENMP
#pragma omp parallel for
#endif
  for (size_t localelement = 0; localelement < rel.nelem; ++localelement) {
    seque<size_t> const &temp = rel.lnods[localelement];
    for (size_t node : temp) {
#ifdef _OPENMP
#pragma omp atomic
#endif
      counts[node]++;
    }
  }
  setnumberofelements(relt, numberofnodes);
#ifdef _OPENMP
#pragma omp parallel for
#endif
  for (size_t localnode = 0; localnode < numberofnodes; ++localnode) {
    setsize(relt.lnods[localnode], counts[localnode]);
  }
  seque<size_t> genmarker(numberofnodes, 0);
  size_t currentgeneration = 1;
  // Outer loop must be sequential because currentgeneration is updated.
  for (size_t element = 0; element < rel.nelem; ++element) {
    seque<size_t> const &temp = rel.lnods[element];
    for (size_t node : temp) {
      if (genmarker[node] != currentgeneration) {
        counts[node] = 0;
        genmarker[node] = currentgeneration;
      }
      relt.lnods[node][counts[node]++] = element;
      relt.maxnodenumber = std::max(relt.maxnodenumber, element);
    }
    currentgeneration++;
  }
}

void multiplication(const o2m &rela, const o2m &relb, o2m &relc) {
  setnumberofelements(relc, rela.nelem);
  relc.maxnodenumber = relb.maxnodenumber;
  seque<size_t> marker(relc.maxnodenumber + 1, 0);
  size_t currentGeneration = 1;
  seque<size_t> temp;
  size_t itemp;
  // Outer loop is sequential due to shared marker and currentGeneration.
  for (size_t aRow = 0; aRow < rela.nelem; ++aRow) {
    itemp = 0;
    seque<size_t> const &rowA = rela.lnods[aRow];
    for (size_t aCol : rowA) {
      if (aCol < relb.nelem) {
        seque<size_t> const &rowB = relb.lnods[aCol];
        for (size_t bCol : rowB) {
          if (marker[bCol] != currentGeneration) {
            marker[bCol] = currentGeneration;
            itemp++;
          }
        }
      }
    }
    setsize(temp, itemp);
    itemp = 0;
    for (size_t aCol : rowA) {
      if (aCol < relb.nelem) {
        seque<size_t> const &rowB = relb.lnods[aCol];
        for (size_t bCol : rowB) {
          if (marker[bCol] == currentGeneration) {
            temp[itemp++] = bCol;
            marker[bCol] = currentGeneration + 1;
          }
        }
      }
    }
    relc.lnods[aRow] = temp;
    currentGeneration += 2;
  }
}

void addition(const o2m &rela, const o2m &relb, o2m &relc) {
  size_t maxelem = std::max(rela.nelem, relb.nelem);
  setnumberofelements(relc, maxelem);
  relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
  seque<size_t> marker(relc.maxnodenumber + 1, 0);
  size_t currentGeneration = 1;
  seque<size_t> temp;
  size_t count;
  // Outer loop sequential due to currentGeneration dependency.
  for (size_t row = 0; row < maxelem; ++row) {
    count = 0;
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
    currentGeneration += 2;
  }
}

void intersection(const o2m &a, const o2m &b, o2m &c) {
  const size_t nRows = std::min(a.nelem, b.nelem);
  setnumberofelements(c, nRows);
  c.maxnodenumber = std::max(a.maxnodenumber, b.maxnodenumber);
#ifdef _OPENMP
#pragma omp parallel for
#endif
  for (size_t r = 0; r < nRows; ++r) {
    seque<size_t> marker(c.maxnodenumber + 1);
    seque<size_t> common;
    size_t currentGeneration = 1;
    const seque<size_t>& rowB = b.lnods[r];
    for (size_t node : rowB) {
      if (marker[node] != currentGeneration)
        marker[node] = currentGeneration;
    }
    size_t count = 0;
    const seque<size_t>& rowA = a.lnods[r];
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
    // currentGeneration increment is local per iteration.
    currentGeneration += 2;
  }
}

void subtraction(const o2m &rela, const o2m &relb, o2m &relc) {
  const size_t nRows = rela.nelem;
  setnumberofelements(relc, nRows);
  relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
#ifdef _OPENMP
#pragma omp parallel for
#endif
  for (size_t r = 0; r < nRows; ++r) {
    seque<size_t> marker(relc.maxnodenumber + 1, 0);
    if (r < relb.nelem) {
      const seque<size_t>& rowB = relb.lnods[r];
      for (size_t node : rowB)
        marker[node] = 1;
    }
    const seque<size_t>& rowA = rela.lnods[r];
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

seque<size_t> toporder(const o2m &rel) {
  seque<size_t> order;
  setsize(order, 0);
  std::vector<size_t> inDegree(getsize(rel.lnods), 0);
#ifdef _OPENMP
#pragma omp parallel for
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
    throw std::runtime_error("The relation contains cycles, topological sort not possible.");
  return order;
}

seque<size_t> lexiorder(const o2m &rel) {
  return getorder(rel.lnods);
}

void indicesfromorder(const o2m &rel, const seque<size_t> &elemOrder,
                      seque<size_t> &oldFromNew, seque<size_t> &newFromOld) {
  indicesfromorder(rel.lnods, elemOrder, oldFromNew, newFromOld);
}

void compresselements(o2m &rel, const seque<size_t> &oldelementfromnew) {
  rel.lnods = rel.lnods(oldelementfromnew);
  rel.nelem = getsize(oldelementfromnew);
  size_t local_max = 0;
#ifdef _OPENMP
#pragma omp parallel for reduction(max:local_max)
#endif
  for (size_t i = 0; i < getsize(rel.lnods); ++i) {
    seque<size_t> const &row = rel.lnods[i];
    for (size_t j = 0; j < getsize(row); ++j) {
      local_max = std::max(local_max, row[j]);
    }
  }
  rel.maxnodenumber = local_max;
}

void permutenodes(o2m &rel, const seque<size_t> &newnodefromold) {
#ifdef _OPENMP
#pragma omp parallel for
#endif
  for (size_t i = 0; i < getsize(rel.lnods); ++i) {
    rel.lnods[i] = newnodefromold(rel.lnods[i]);
  }
  size_t local_max = 0;
#ifdef _OPENMP
#pragma omp parallel for reduction(max:local_max)
#endif
  for (size_t i = 0; i < getsize(rel.lnods); ++i) {
    seque<size_t> const &row = rel.lnods[i];
    for (size_t j = 0; j < getsize(row); ++j) {
      local_max = std::max(local_max, row[j]);
    }
  }
  rel.maxnodenumber = local_max;
}

} // namespace hidden
