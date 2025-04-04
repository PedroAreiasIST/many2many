#ifndef RELMANYTOMANY_HPP
#define RELMANYTOMANY_HPP

#include <cstddef>

#include "godoftypes.hpp"
#include "o2m.hpp"
#include "seque.hpp"

namespace hidden
{
    using lst = seque<size_t>;
    using lst2 = seque<lst>;
} // namespace hidden

struct m2m
{
    hidden::o2m nodesfromelement;
    hidden::o2m elementsfromnode;
    seque<seque<size_t>> nodelocation;
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
seque<size_t> getelementswithnodes(m2m const &rel, seque<size_t> const &nodes);
seque<size_t> getelementsfromnodes(m2m const &rel, seque<size_t> const &nodes);
seque<size_t> getelementneighbours(m2m const &rel, size_t element);
seque<size_t> getnodeneighbours(m2m const &rel, size_t node)
;
seque<size_t> lexiorder(m2m const &rel);
seque<size_t> toporder(m2m const &rel);
void indicesfromorder(m2m const &rel, const seque<size_t> &elementorder, seque<size_t> &oldfromnew,
                      seque<size_t> &newfromold);
void compresselements(m2m &rel, seque<size_t> const &oldelementfromnew);
void permutenodes(m2m &rel, seque<size_t> const &newnodefromold);
void getelementstoelements(m2m const &rel, m2m &elementstoelements);
void getnodestonodes(m2m const &rel, m2m &nodestonodes);
size_t getlocalnodeposition(m2m const &rel, size_t node, size_t localelement);

#endif
