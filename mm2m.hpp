#ifndef RELATIONMATRIX_HPP
#define RELATIONMATRIX_HPP
#include <map>
#include <stack>
#include <utility>
#include "m2m.hpp"
#include "symmetries.hpp"
namespace hidden
{
    using matrix = seque<seque<m2m>>;
}
struct mm2m
{
    hidden::matrix m{};
    seque<seque<seque<seque<size_t>>>> groups{{}};
    size_t ntypes{0};
    seque<std::pair<size_t, size_t>> listofmarked;
    m2m const &operator()(size_t elementtype, size_t nodetype) const { return m[elementtype][nodetype]; }
    m2m &operator()(size_t elementtype, size_t nodetype) { return m[elementtype][nodetype]; }
    size_t nnodes(size_t elementtype, size_t element, size_t nodetype);
    size_t nelems(size_t nodetype, size_t node, size_t elementtype);
};
PFR_FUNCTIONS_FOR(mm2m)
void resetmarked(mm2m &m);
void marktoerase(mm2m &m, std::pair<size_t, size_t> const &node);
seque<std::pair<size_t, size_t>> getallelements(mm2m &m, std::pair<size_t, size_t> const &node);
seque<std::pair<size_t, size_t>> getallnodes(mm2m &m, std::pair<size_t, size_t> const &element);
seque<std::pair<size_t, size_t>> depthfirstsearchfromanode(mm2m &m, std::pair<size_t, size_t> const &node);
void setnumberoftypes(mm2m &m, size_t ntypes);
void setsymmetrygroup(mm2m &m, size_t elementype, size_t nodetype, seque<seque<size_t>> const &group);
size_t appendelement(mm2m &m, size_t elementype, size_t nodetype, seque<size_t> const &nodes);
size_t appendelement(mm2m &m,size_t elementype)
;
void setmany2many(mm2m &m, size_t elementype, size_t nodetype, m2m const &relation);
m2m &getmany2many(mm2m &m, size_t elementype, size_t nodetype);
void indicesfromorder(mm2m &m, size_t elementtype, size_t nodetype, seque<size_t> const &order,
                      seque<size_t> &oldfromnew, seque<size_t> &newfromold);
void closeeverything(mm2m &m);
seque<size_t> getselementsfromnodes(mm2m &matrix, size_t elementtype, size_t nodestype, seque<size_t> const &nodes);
namespace hidden
{
    void compress(mm2m &m, size_t elementtype, seque<size_t> const &oldelementfromnew,
                  seque<size_t> const &newelementfromold);
}
void compress(mm2m &m);
seque<size_t> typetoporder(mm2m const &m);


#endif // RELATIONMATRIX_HPP
