#ifndef RELATIONMATRIX_HPP
#define RELATIONMATRIX_HPP
#include <map>
#include <stack>
#include <utility>
#include "many2many.hpp"
#include "symmetries.hpp"
namespace hidden
{
    using matrix = seque<many2many>;
}
struct relationmatrix
{
    hidden::matrix m{};
    seque<seque<seque<seque<size_t>>>> groups{{}};
    size_t ntypes{0};
    seque<std::pair<size_t, size_t>> listofmarked;
    many2many const &operator()(size_t elementtype, size_t nodetype) const
    {
        size_t minimum = std::min(nodetype, elementtype);
        size_t maximum = std::max(nodetype, elementtype);
        size_t index = minimum * ntypes - (minimum - 1) * minimum / 2 + maximum - minimum;
        return m[index];
    }
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
void resetmarked(relationmatrix &m);
void marktoerase(relationmatrix &m, std::pair<size_t, size_t> const &node);
seque<std::pair<size_t, size_t>> getallelements(relationmatrix &m, std::pair<size_t, size_t> const &node);
seque<std::pair<size_t, size_t>> getallnodes(relationmatrix &m, std::pair<size_t, size_t> const &element);
seque<std::pair<size_t, size_t>> depthfirstsearchfromanode(relationmatrix &m, std::pair<size_t, size_t> const &node);
void setnumberoftypes(relationmatrix &m, size_t ntypes);
void setsymmetrygroup(relationmatrix &m, size_t elementype, size_t nodetype, seque<seque<size_t>> const &group);
size_t appendelement(relationmatrix &m, size_t elementype, size_t nodetype, seque<size_t> const &nodes);
void setmany2many(relationmatrix &m, size_t elementype, size_t nodetype, many2many const &relation);
many2many &getmany2many(relationmatrix &m, size_t elementype, size_t nodetype);
void lexiorder(relationmatrix &m, size_t elementype, size_t nodetype, seque<size_t> &order);
void indicesfromorder(relationmatrix &m, size_t elementtype, size_t nodetype, seque<size_t> const &order,
                      seque<size_t> &oldfromnew, seque<size_t> &newfromold);
void closeeverything(relationmatrix &m);
seque<size_t> getselementsfromnodes(relationmatrix &matrix, size_t elementtype, size_t nodestype,
                                    seque<size_t> const &nodes);
void compress(relationmatrix &m, size_t elementtype, seque<size_t> const &oldelementfromnew,
              seque<size_t> const &newelementfromold);
void compress(relationmatrix &m);
void typetoporder(relationmatrix const &m, seque<size_t> &order);


#endif // RELATIONMATRIX_HPP
