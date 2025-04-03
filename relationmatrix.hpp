#ifndef RELATIONMATRIX_HPP
#define RELATIONMATRIX_HPP
#include <map>
#include <stack>
#include <utility>
#include "many2many.hpp"
#include "symmetries.hpp"
namespace hidden
{
    using matrix = sequence<many2many>;
}
struct relationmatrix
{
    hidden::matrix m{};
    sequence<sequence<sequence<sequence<size_t>>>> groups{{}};
    size_t ntypes{0};
    sequence<std::pair<size_t, size_t>> listofmarked;
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
sequence<std::pair<size_t, size_t>> getallelements(relationmatrix &m, std::pair<size_t, size_t> const &node);
sequence<std::pair<size_t, size_t>> getallnodes(relationmatrix &m, std::pair<size_t, size_t> const &element);
sequence<std::pair<size_t, size_t>> depthfirstsearchfromanode(relationmatrix &m, std::pair<size_t, size_t> const &node);
void setnumberoftypes(relationmatrix &m, size_t ntypes);
void setsymmetrygroup(relationmatrix &m, size_t elementype, size_t nodetype, sequence<sequence<size_t>> const &group);
size_t appendelement(relationmatrix &m, size_t elementype, size_t nodetype, sequence<size_t> const &nodes);
void setmany2many(relationmatrix &m, size_t elementype, size_t nodetype, many2many const &relation);
many2many &getmany2many(relationmatrix &m, size_t elementype, size_t nodetype);
void lexiorder(relationmatrix &m, size_t elementype, size_t nodetype, sequence<size_t> &order);
void indicesfromorder(relationmatrix &m, size_t elementtype, size_t nodetype, sequence<size_t> const &order,
                      sequence<size_t> &oldfromnew, sequence<size_t> &newfromold);
void closeeverything(relationmatrix &m);
sequence<size_t> getselementsfromnodes(relationmatrix &matrix, size_t elementtype, size_t nodestype,
                                       sequence<size_t> const &nodes);
void compress(relationmatrix &m, size_t elementtype, sequence<size_t> const &oldelementfromnew,
              sequence<size_t> const &newelementfromold);
void compress(relationmatrix &m);


#endif // RELATIONMATRIX_HPP
