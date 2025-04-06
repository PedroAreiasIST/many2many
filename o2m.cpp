#include "o2m.hpp"
#include <algorithm>
#include <cassert>
#include <queue>
#include <stdexcept>
#include <vector>
#ifdef _OPENMP
#include <omp.h>
#endif

namespace hidden {

  size_t update_max_for_nodes(seque<size_t> const &nodes, size_t current_max) {
    size_t max_val = current_max;
#ifdef _OPENMP
#pragma omp parallel for reduction(max : max_val)
#endif
    for (size_t i = 0; i < getsize(nodes); ++i)
      max_val = std::max(max_val, nodes[i]);
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
#pragma omp parallel for reduction(max : local_max)
#endif
    for (size_t i = 0; i < getsize(nodes); ++i)
      local_max = std::max(local_max, nodes[i]);
    rel.maxnodenumber = local_max;
  }

  size_t appendelement(o2m &rel, seque<size_t> const &nodes) {
    rel.nelem++;
    setsize(rel.lnods, rel.nelem);
    assert(getsize(rel.lnods) == rel.nelem);
    rel.lnods[rel.nelem - 1] = nodes;
    size_t local_max = rel.maxnodenumber;
#ifdef _OPENMP
#pragma omp parallel for reduction(max : local_max)
#endif
    for (size_t i = 0; i < getsize(nodes); ++i)
      local_max = std::max(local_max, nodes[i]);
    rel.maxnodenumber = local_max;
    return rel.nelem - 1;
  }

  void transpose(const o2m &rel, o2m &relt) {
    relt.maxnodenumber = 0;
    relt.nelem = 0;
    erase(relt.lnods);
    const size_t numberofnodes = rel.maxnodenumber + 1;
    std::vector<size_t> counts(numberofnodes, 0);
    for (size_t localelement = 0; localelement < getsize(rel.lnods); ++localelement) {
      seque<size_t> temp = rel.lnods[localelement];
      for (size_t node : temp)
        counts[node]++;
    }
    setnumberofelements(relt, numberofnodes);
    for (size_t localnode = 0; localnode < numberofnodes; ++localnode)
      setsize(relt.lnods[localnode], counts[localnode]);
    std::vector<size_t> genmarker(numberofnodes, 0);
    size_t currentgeneration = 1;
    for (size_t element = 0; element < rel.nelem; ++element) {
      seque<size_t> temp = rel.lnods[element];
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
    std::vector<size_t> marker(relc.maxnodenumber + 1, 0);
    unsigned int currentGeneration = 1;
    std::vector<size_t> temp;
    for (size_t aRow = 0; aRow < rela.nelem; ++aRow) {
      temp.clear();
      seque<size_t> rowA = rela.lnods[aRow];
      for (size_t aCol : rowA) {
        if (aCol < getsize(relb.lnods)) {
          seque<size_t> rowB = relb.lnods[aCol];
          for (size_t bCol : rowB)
            if (marker[bCol] != currentGeneration) {
              marker[bCol] = currentGeneration;
              temp.push_back(bCol);
            }
        }
      }
      std::sort(temp.begin(), temp.end());
      setsize(relc.lnods[aRow], temp.size());
      for (size_t i = 0; i < temp.size(); ++i)
        relc.lnods[aRow][i] = temp[i];
      ++currentGeneration;
    }
  }

  void addition(const o2m &rela, const o2m &relb, o2m &relc) {
    size_t maxelem = std::max(rela.nelem, relb.nelem);
    setnumberofelements(relc, maxelem);
    relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
    std::vector<size_t> marker(relc.maxnodenumber + 1, 0);
    std::vector<size_t> temp;
    unsigned int currentGeneration = 1;
    for (size_t row = 0; row < maxelem; ++row) {
      temp.clear();
      if (row < rela.nelem) {
        seque<size_t> rowA = rela.lnods[row];
        for (size_t node : rowA)
          if (marker[node] != currentGeneration) {
            marker[node] = currentGeneration;
            temp.push_back(node);
          }
      }
      if (row < relb.nelem) {
        seque<size_t> rowB = relb.lnods[row];
        for (size_t node : rowB)
          if (marker[node] != currentGeneration) {
            marker[node] = currentGeneration;
            temp.push_back(node);
          }
      }
      std::sort(temp.begin(), temp.end());
      setsize(relc.lnods[row], temp.size());
      for (size_t i = 0; i < temp.size(); ++i)
        relc.lnods[row][i] = temp[i];
      ++currentGeneration;
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
      seque<size_t> rowA = a.lnods[r];
      seque<size_t> rowB = b.lnods[r];
      std::vector<size_t> local_marker(c.maxnodenumber + 1, 0);
      for (size_t node : rowB)
        local_marker[node] = 1;
      std::vector<size_t> common;
      for (size_t node : rowA)
        if (local_marker[node] == 1)
          common.push_back(node);
      std::sort(common.begin(), common.end());
      setsize(c.lnods[r], common.size());
      for (size_t i = 0; i < common.size(); ++i)
        c.lnods[r][i] = common[i];
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
      seque<size_t> rowA = rela.lnods[r];
      std::vector<size_t> local_marker(relc.maxnodenumber + 1, 0);
      if (r < relb.nelem) {
        seque<size_t> rowB = relb.lnods[r];
        for (size_t node : rowB)
          local_marker[node] = 1;
      }
      std::vector<size_t> diff;
      for (size_t node : rowA)
        if (local_marker[node] == 0)
          diff.push_back(node);
      std::sort(diff.begin(), diff.end());
      setsize(relc.lnods[r], diff.size());
      for (size_t i = 0; i < diff.size(); ++i)
        relc.lnods[r][i] = diff[i];
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
      seque<size_t> row = rel.lnods[i];
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
      for (size_t nbr : rel.lnods[cur])
        if (--inDegree[nbr] == 0)
          q.push(nbr);
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
#pragma omp parallel for reduction(max : local_max)
#endif
    for (size_t i = 0; i < getsize(rel.lnods); ++i) {
      seque<size_t> row = rel.lnods[i];
      for (size_t j = 0; j < getsize(row); ++j)
        local_max = std::max(local_max, row[j]);
    }
    rel.maxnodenumber = local_max;
  }

  void permutenodes(o2m &rel, const seque<size_t> &newnodefromold) {
#ifdef _OPENMP
      std::cout<<"OMP"<<std::endl;
#pragma omp parallel for
#endif
    for (size_t i = 0; i < getsize(rel.lnods); ++i) {
      rel.lnods[i] = newnodefromold(rel.lnods[i]);
    }
    size_t local_max = 0;
#ifdef _OPENMP
#pragma omp parallel for reduction(max : local_max)
#endif
    for (size_t i = 0; i < getsize(rel.lnods); ++i) {
      seque<size_t> row = rel.lnods[i];
      for (size_t j = 0; j < getsize(row); ++j)
        local_max = std::max(local_max, row[j]);
    }
    rel.maxnodenumber = local_max;
  }

}
