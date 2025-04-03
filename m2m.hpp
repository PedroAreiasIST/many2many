#ifndef RELMANYTOMANY_HPP
#define RELMANYTOMANY_HPP

#include <cstddef>

#include "godoftypes.hpp"
#include "one2many.hpp"
#include "seque.hpp"

namespace hidden
{
    using lst = seque<size_t>;
    using lst2 = seque<lst>;
} // namespace hidden

struct m2m
{
    hidden::one2many nodesfromelement;
    hidden::one2many elementsfromnode;
    hidden::lst2 nodelocation;
    size_t nnodes(size_t element);
    size_t nelems(size_t node);
};
PFR_FUNCTIONS_FOR(m2m)
void setnumberofelements(m2m &rel, size_t nelem);
void setnodesforelement(m2m &rel, size_t element, seque<size_t> const &nodes);
size_t appendelement(m2m &rel, seque<size_t> const &nodes);
void multiplication(const m2m &rela, bool transposea, const m2m &relb, bool transposeb, m2m &relc);
void addition(const m2m &rela, bool transposea, const m2m &relb, bool transposeb, m2m &relc);
void intersection(const m2m &rela, bool transposea, const m2m &relb, bool transposeb, m2m &relc);
void subtraction(const m2m &rela, bool transposea, const m2m &relb, bool transposeb, m2m &relc);
void setallpointers(m2m &rel);
seque<size_t> getelementsfromnodes(m2m const &rel, hidden::lst const &nodes);
seque<size_t> getneighbours(m2m const &rel, size_t element);
seque<size_t> lexiorder(m2m const &rel);
seque<size_t> toporder(m2m const &rel);
void indicesfromorder(m2m const &rel, const hidden::lst &elementorder, hidden::lst &oldfromnew,
                      hidden::lst &newfromold);
void compresselements(m2m &rel, hidden::lst const &oldelementfromnew);
void permutenodes(m2m &rel, hidden::lst const &newnodefromold);
void getelementstoelements(m2m const &rel, m2m &elementstoelements);
void getnodestonodes(m2m const &rel, m2m &nodestonodes);
size_t getlocalnodeposition(m2m const &rel, size_t node, size_t localelement);

#endif
