#ifndef RELMANYTOMANY_HPP
#define RELMANYTOMANY_HPP

#include <cstddef>
#include "relationonetomany.hpp"
#include "sek.hpp"

namespace
{
    using lst = sek<size_t>;
    using lst2 = sek<lst>;
} // namespace

struct relmanytomany
{
    relationonetomany nodesfromelement;
    relationonetomany elementsfromnode;
    lst2 nodelocation;
    lst2 elementlocation;
    size_t nnodes(size_t element);
    size_t nelems(size_t node);
};
PFR_FUNCTIONS_FOR(relmanytomany)

void setfromonetomany(relmanytomany &rel);
lst getelementsfromnodes(relmanytomany const &rel, lst const &nodes);
lst getneighbours(relmanytomany const &rel, size_t element);
void lexiorder(relmanytomany const &rel, lst &orderofelements);
void toporder(relmanytomany const &rel, bool transpose, lst &order);
void indicesfromorder(relmanytomany const &rel, const lst &elementorder, lst &oldfromnew, lst &newfromold);
void compresselements(relmanytomany &rel, lst const &oldelementfromnew);
void compressnodes(relmanytomany &rel, lst const &newnodefromold);
size_t getlocalnodeposition(relmanytomany const &rel, size_t node, size_t localelement);
size_t getlocalelementposition(relmanytomany const &rel, size_t element, size_t localnode);
#endif
