#ifndef RELMANYTOMANY_HPP
#define RELMANYTOMANY_HPP

#include <cstddef>
#include "relationonetomany.hpp"
#include "sek.hpp"

namespace hidden
{
    using lst = sek<size_t>;
    using lst2 = sek<lst>;
} // namespace hidden

struct relmanytomany
{
    relationonetomany nodesfromelement;
    relationonetomany elementsfromnode;
    hidden::lst2 nodelocation;
    hidden::lst2 elementlocation;
    size_t nnodes(size_t element);
    size_t nelems(size_t node);
};
PFR_FUNCTIONS_FOR(relmanytomany)

void setnodesfromelement(relmanytomany &rel, relationonetomany const &nodesfromelement);
void setallpointers(relmanytomany &rel);
hidden::lst getelementsfromnodes(relmanytomany const &rel, hidden::lst const &nodes);
hidden::lst getneighbours(relmanytomany const &rel, size_t element);
void lexiorder(relmanytomany const &rel, hidden::lst &orderofelements);
void toporder(relmanytomany const &rel, bool transpose, hidden::lst &order);
void indicesfromorder(relmanytomany const &rel, const hidden::lst &elementorder, hidden::lst &oldfromnew,
                      hidden::lst &newfromold);
void compresselements(relmanytomany &rel, hidden::lst const &oldelementfromnew);
void compressnodes(relmanytomany &rel, hidden::lst const &newnodefromold);
void getelementstoelements(relmanytomany const &rel, relmanytomany &elementstoelements);
void getnodestonodes(relmanytomany const &rel, relmanytomany &nodestonodes);
;
size_t getlocalnodeposition(relmanytomany const &rel, size_t node, size_t localelement);
size_t getlocalelementposition(relmanytomany const &rel, size_t element, size_t localnode);
#endif
