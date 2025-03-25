#ifndef RELATIONMATRIX_HPP
#define RELATIONMATRIX_HPP
#include <stack>
#include <utility>
#include "relationmodel.hpp"
#include "relmanytomany.hpp"
/**
 * @brief relationmatrix structure
 */
namespace hidden
{
    using matrix = sek<sek<relmanytomany>>;
}

struct relationmatrix
{
    hidden::matrix m;
    relmanytomany &operator()(size_t elementtype, size_t nodetype) { return m[elementtype][nodetype]; }
    size_t nnode(size_t elementtype, size_t element, size_t nodetype)
    {
        return getsize(m[elementtype][nodetype].nodesfromelement.onetomany[element]);
    }
    size_t nelem(size_t nodetype, size_t node, size_t elementtype)
    {
        return getsize(m[elementtype][nodetype].elementsfromnode.onetomany[node]);
    }
    sek<size_t> &operator()(std::pair<size_t, size_t> element, size_t nodetype)
    {
        return m[element.first][nodetype].nodesfromelement.onetomany[element.second];
    }
};
PFR_FUNCTIONS_FOR(relationmatrix)

void setnumberoftypes(relationmatrix &m, size_t ntypes);

size_t appendelement(relationmatrix &m, size_t elementype, size_t nodetype, sek<size_t> const &nodes);

void lexiorder(relationmatrix &m, size_t elementype, size_t nodetype, sek<size_t> &order);

void indicesfromorder(relationmatrix &m, size_t elementtype, size_t nodetype, sek<size_t> const &order,
                      sek<size_t> &oldfromnew, sek<size_t> &newfromold);

void closeelementnoderelation(relationmatrix &m, size_t elementype, size_t nodetype);

sek<size_t> getselementsfromnodes(relationmatrix &matrix, size_t elementtype, size_t nodestype,
                                  sek<size_t> const &nodes);

void getnodesfromanelement(relationmatrix &matrix, size_t elementtype, size_t element, size_t nodestype,
                           sek<size_t> &nodes);

void getelementsfromnode(relationmatrix &matrix, size_t nodetype, size_t node, size_t elementstype,
                         sek<size_t> &elements);

void compress(relationmatrix &m, size_t elementtype, sek<size_t> const &oldelementfromnew,
              sek<size_t> const &newelementfromold);

void getnodes(relationmatrix &matrix, size_t type1, size_t element1, size_t type2, sek<size_t> &nodelist);

void getsallnodes(relationmatrix &matrix, size_t elementtype, size_t element, sek<sek<size_t>> &nodes);

#endif // RELATIONMATRIX_HPP
