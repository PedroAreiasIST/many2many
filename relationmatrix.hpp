#ifndef RELATIONMATRIX_HPP
#define RELATIONMATRIX_HPP
#include <map>
#include <stack>
#include <utility>
#include "many2many.hpp"
#include "symmetries.hpp"
namespace hidden
{
    using matrix = sek<many2many>;
}
struct relationmatrix
{
    hidden::matrix m{};
    sek<sek<sek<sek<size_t>>>> groups{{}};
    size_t ntypes{0};
    sek<std::pair<size_t, size_t>> listofmarked;
    many2many &operator()(size_t elementtype, size_t nodetype)
    {
        size_t minimum = std::min(nodetype, elementtype);
        size_t maximum = std::max(nodetype, elementtype);
        size_t index = minimum * ntypes - (minimum - 1) * minimum / 2 + maximum - minimum;
        return m[index];
    }
    size_t nnode(size_t elementtype, size_t element, size_t nodetype);
    size_t nelem(size_t nodetype, size_t node, size_t elementtype);
};
PFR_FUNCTIONS_FOR(relationmatrix)
void cleanmarked(relationmatrix &m);
void marktoerase(relationmatrix &m, std::pair<size_t, size_t> const &node);
sek<std::pair<size_t, size_t>> getallelements(relationmatrix &m, std::pair<size_t, size_t> const &node);
sek<std::pair<size_t, size_t>> getallnodes(relationmatrix &m, std::pair<size_t, size_t> const &element);
sek<std::pair<size_t, size_t>> depthfirstsearchfromanode(relationmatrix &m, std::pair<size_t, size_t> const &node);
void setnumberoftypes(relationmatrix &m, size_t ntypes);
void setsymmetrygroup(relationmatrix &m, size_t elementype, size_t nodetype, sek<sek<size_t>> const &group);
size_t appendelement(relationmatrix &m, size_t elementype, size_t nodetype, sek<size_t> const &nodes);
void setmany2many(relationmatrix &m, size_t elementype, size_t nodetype, many2many const &relation);
many2many &getmany2many(relationmatrix &m, size_t elementype, size_t nodetype);
void lexiorder(relationmatrix &m, size_t elementype, size_t nodetype, sek<size_t> &order);
void indicesfromorder(relationmatrix &m, size_t elementtype, size_t nodetype, sek<size_t> const &order,
                      sek<size_t> &oldfromnew, sek<size_t> &newfromold);
void closeeverything(relationmatrix &m);
sek<size_t> getselementsfromnodes(relationmatrix &matrix, size_t elementtype, size_t nodestype,
                                  sek<size_t> const &nodes);
void compress(relationmatrix &m, size_t elementtype, sek<size_t> const &oldelementfromnew,
              sek<size_t> const &newelementfromold);

#endif // RELATIONMATRIX_HPP
