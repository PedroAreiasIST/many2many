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
  m2m const &operator()(int elementtype, int nodetype) const;
  m2m &operator()(int elementtype, int nodetype);
  int nnodes(int elementtype, int element, int nodetype) const;
  int nelems(int nodetype, int node, int elementtype) const;
  int nelems(int elementtype);
  int nactiveelements(int elementtype);
};

PFR_FUNCTIONS_FOR(mm2m)

void marktoerase(mm2m &m, int nodetype, int node);

void marktoeraserepeated(mm2m &m, int elementtype, int nodetype);

seque<std::pair<int, int>> getallelements(mm2m const &m, int nodetype);

seque<std::pair<int, int>> getallelements(mm2m const &m, int nodetype,
                                          int node);

seque<std::pair<int, int>> getallnodes(mm2m const &m, int elementtype,
                                       int element);

seque<std::pair<int, int>> getallnodes(mm2m const &m, int elementtype);

namespace hidden {
seque<std::pair<int, int>>
depthfirstsearchfromanode(mm2m const &m, std::pair<int, int> const &node);
}

void setnumberoftypes(mm2m &m, int ntypes);

int appendelement(mm2m &m, int elementype, int nodetype,
                  seque<int> const &nodes);

seque<int> getelementsfromnodes(mm2m &matrix, int elementtype, int nodestype,
                                seque<int> const &nodes);

seque<int> getelementswithnodes(mm2m &matrix, int elementtype, int nodestype,
                                seque<int> const &nodes);

void setcompressed(mm2m &m);

seque<int> gettypetoporder(mm2m const &m);

#endif // RELATIONMATRIX_HPP
