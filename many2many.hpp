#ifndef RELMANYTOMANY_HPP
#define RELMANYTOMANY_HPP

#include <cstddef>
#include "one2many.hpp"
#include "seque.hpp"

namespace hidden
{
    using lst = seque<size_t>;
    using lst2 = seque<lst>;
} // namespace hidden

struct many2many
{
    hidden::one2many nodesfromelement;
    hidden::one2many elementsfromnode;
    hidden::lst2 nodelocation;
    size_t nnodes(size_t element);
    size_t nelems(size_t node);
};
PFR_FUNCTIONS_FOR(many2many)
void setnelem(many2many &rel, size_t nelem);
void setnnodes(many2many &rel, size_t element, size_t nnodes);
void setnodes(many2many &rel, size_t element, seque<size_t> const &nodes);
size_t appendelement(many2many &rel, seque<size_t> const &nodes);
void multiplication(const many2many &rela, bool transposea, const many2many &relb, bool transposeb, many2many &relc);
void addition(const many2many &rela, bool transposea, const many2many &relb, bool transposeb, many2many &relc);
void intersection(const many2many &rela, bool transposea, const many2many &relb, bool transposeb, many2many &relc);
void difference(const many2many &rela, bool transposea, const many2many &relb, bool transposeb, many2many &relc);
void setallpointers(many2many &rel);
hidden::lst getelementsfromnodes(many2many const &rel, hidden::lst const &nodes);
hidden::lst getneighbours(many2many const &rel, size_t element);
void lexiorder(many2many const &rel, hidden::lst &orderofelements);
void toporder(many2many const &rel, hidden::lst &orderofelements);
void indicesfromorder(many2many const &rel, const hidden::lst &elementorder, hidden::lst &oldfromnew,
                      hidden::lst &newfromold);
void compresselements(many2many &rel, hidden::lst const &oldelementfromnew);
void compressnodes(many2many &rel, hidden::lst const &newnodefromold);
void getelementstoelements(many2many const &rel, many2many &elementstoelements);
void getnodestonodes(many2many const &rel, many2many &nodestonodes);
size_t getlocalnodeposition(many2many const &rel, size_t node, size_t localelement);
#endif
