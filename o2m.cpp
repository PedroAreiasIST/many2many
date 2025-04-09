#include "o2m.hpp"
#include <algorithm>
#include <cassert>
#include <execution>
#include <numeric>
#include <omp.h>
#include <queue>
#include <stdexcept>
#include <vector>
namespace hidden {

size_t update_max_for_nodes(seque<size_t> const &nodes, size_t current_max) {
  return std::reduce(std::execution::par, nodes.begin(), nodes.end(),
                     current_max,
                     [](size_t a, size_t b) { return std::max(a, b); });
}

void setnumberofelements(o2m &rel, size_t nelem) {
  rel.nelem = nelem;
  setsize(rel.lnods, nelem);
}

void setnodesforelement(o2m &rel, size_t element, seque<size_t> const &nodes) {
  rel.lnods[element] = nodes;
  for (size_t i = 0; i < getsize(nodes); ++i)
    rel.maxnodenumber = std::max(rel.maxnodenumber, nodes[i]);
}

size_t appendelement(o2m &rel, seque<size_t> const &nodes) {
  rel.nelem++;
  setsize(rel.lnods, rel.nelem);
  assert(getsize(rel.lnods) == rel.nelem);
  rel.lnods[rel.nelem - 1] = nodes;
  rel.maxnodenumber = std::reduce(
      std::execution::par, nodes.begin(), nodes.end(), rel.maxnodenumber,
      [](size_t a, size_t b) { return std::max(a, b); });
  return rel.nelem - 1;
}

#include <vector>
#ifdef _OPENMP
#include <omp.h>
#endif

void transpose(const o2m &rel, o2m &relt) {
  relt.maxnodenumber = 0;
  relt.nelem = 0;
  erase(relt.lnods);
  if (rel.nelem == 0) {
    return;
  }
  const size_t numberOfNodes = rel.maxnodenumber + 1;
  std::vector<size_t> counts(numberOfNodes, 0);
#pragma omp parallel for default(none) shared(rel, counts)
  for (size_t elementIdx = 0; elementIdx < rel.nelem; ++elementIdx) {
    for (size_t nodeId : rel.lnods[elementIdx]) {
#pragma omp atomic
      counts[nodeId]++;
    }
  }
  setnumberofelements(relt, numberOfNodes);
  for (size_t node = 0; node < numberOfNodes; ++node) {
    setsize(relt.lnods[node], counts[node]);
  }
  std::vector<size_t> offsets(numberOfNodes, 0);
#pragma omp parallel for default(none) shared(rel, relt, offsets)
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
}

#include <vector>
#ifdef _OPENMP
#include <omp.h>
#endif

void multiplication(const o2m &rela, const o2m &relb, o2m &relc) {
  setnumberofelements(relc, rela.nelem);
  relc.maxnodenumber = relb.maxnodenumber;
  if (rela.nelem == 0 || relb.maxnodenumber == 0)
    return;
#pragma omp parallel
  {
    seque<size_t> marker(relb.maxnodenumber + 1, 0);
    seque<size_t> resultNodes;
    size_t generation = 1;
#pragma omp for schedule(static)
    for (size_t elementAIndex = 0; elementAIndex < rela.nelem;
         ++elementAIndex) {
      const seque<size_t> &elementA = rela.lnods[elementAIndex];
      size_t currentGeneration = generation;
      generation += 2;
      size_t estCapacity = 0;
      for (size_t nodeA : elementA) {
        if (nodeA < relb.nelem)
          estCapacity += getsize(relb.lnods[nodeA]);
      }
      setsize(resultNodes, estCapacity);
      size_t resultCount = 0;
      for (size_t nodeA : elementA) {
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
          if (marker[nodeB] != currentGeneration) {
            marker[nodeB] = currentGeneration;
            resultNodes[resultCount++] = nodeB;
          }
        }
      }
      setsize(resultNodes, resultCount);
      relc.lnods[elementAIndex] = resultNodes;
    }
  }
}

void addition(const o2m &rela, const o2m &relb, o2m &relc) {
  size_t maxElements = std::max(rela.nelem, relb.nelem);
  setnumberofelements(relc, maxElements);
  relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
#ifdef _OPENMP
#pragma omp parallel
  {
    seque<size_t> marker(relc.maxnodenumber + 1, 0);
#pragma omp for schedule(dynamic)
    for (size_t element = 0; element < maxElements; ++element) {
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
    }
  }
#else
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

void intersection(const o2m &a, const o2m &b, o2m &c) {
  const size_t nElements = std::min(a.nelem, b.nelem);
  setnumberofelements(c, nElements);
  c.maxnodenumber = std::max(a.maxnodenumber, b.maxnodenumber);
#ifdef _OPENMP
#pragma omp parallel
  {
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

void subtraction(const o2m &rela, const o2m &relb, o2m &relc) {
  const size_t nElements = rela.nelem;
  setnumberofelements(relc, nElements);
  relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
#ifdef _OPENMP
#pragma omp parallel
  {
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

} // namespace hidden