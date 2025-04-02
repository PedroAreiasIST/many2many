#ifndef RELATIONONETOMANY_HPP
#define RELATIONONETOMANY_HPP

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <queue>
#include "sek.hpp"

namespace hidden
{
    using lst = sek<size_t>;
    using lst2 = sek<lst>;
} // namespace hidden

struct relationonetomany
{
    hidden::lst2 lnods;
    size_t nelem{0};
    size_t maxnodenumber{0};
};

PFR_FUNCTIONS_FOR(relationonetomany)

void setnelem(relationonetomany &rel, size_t nelem);
size_t appendelement(relationonetomany &rel, const hidden::lst &nodes);
void transpose(const relationonetomany &rel, relationonetomany &relt);
void times(const relationonetomany &rela, const relationonetomany &relb, relationonetomany &relc);
void plusunion(const relationonetomany &rela, const relationonetomany &relb, relationonetomany &relc);
void intersection(const relationonetomany &a, const relationonetomany &b, relationonetomany &c);
void difference(const relationonetomany &rela, const relationonetomany &relb, relationonetomany &relc);
void toporder(const relationonetomany &rel, hidden::lst &order);
void lexiorder(const relationonetomany &rel, hidden::lst &orderofelements);
void indicesfromorder(const relationonetomany &rel, const hidden::lst &elemOrder, hidden::lst &oldFromNew,
                      hidden::lst &newFromOld);
void compresselements(relationonetomany &rel, const hidden::lst &oldelementfromnew);
void compressnodes(relationonetomany &rel, const hidden::lst &newnodefromold);

#endif
