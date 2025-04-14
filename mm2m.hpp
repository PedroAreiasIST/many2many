#ifndef RELATIONMATRIX_HPP
#define RELATIONMATRIX_HPP
#include "m2m.hpp"
#include "symmetries.hpp"
#include <map>
#include <stack>
#include <utility>

struct mm2m {
  seque<seque<m2m>> m{};
  int ntypes{0};
  seque<std::pair<int, int>> listofmarked;

  m2m const &operator()(int elementtype, int nodetype) const {
    return m[elementtype][nodetype];
  }

  m2m &operator()(int elementtype, int nodetype) {
    return m[elementtype][nodetype];
  }

  int nnodes(int elementtype, int element, int nodetype);

  int nelems(int nodetype, int node, int elementtype);
};

PFR_FUNCTIONS_FOR(mm2m)

void resetmarked(mm2m &m);

void marktoerase(mm2m &m, std::pair<int, int> const &node);

seque<std::pair<int, int>> getallelements(mm2m &m,
                                          std::pair<int, int> const &node);

seque<std::pair<int, int>> getallnodes(mm2m &m,
                                       std::pair<int, int> const &element);

seque<std::pair<int, int>>
depthfirstsearchfromanode(mm2m &m, std::pair<int, int> const &node);

void setnumberoftypes(mm2m &m, int ntypes);

int appendelement(mm2m &m, int elementype, int nodetype,
                  seque<int> const &nodes);

int appendelement(mm2m &m, int elementype);

void setmany2many(mm2m &m, int elementype, int nodetype, m2m const &relation);

m2m &getmany2many(mm2m &m, int elementype, int nodetype);

void indicesfromorder(mm2m &m, int elementtype, int nodetype,
                      seque<int> const &order, seque<int> &oldfromnew,
                      seque<int> &newfromold);

void closeeverything(mm2m &m);

seque<int> getselementsfromnodes(mm2m &matrix, int elementtype, int nodestype,
                                 seque<int> const &nodes);

namespace hidden {
void compress(mm2m &m, int elementtype, seque<int> const &oldelementfromnew,
              seque<int> const &newelementfromold);
}

void compress(mm2m &m);

seque<int> typegettoporder(mm2m const &m);

#endif // RELATIONMATRIX_HPP
