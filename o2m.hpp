#ifndef RELATIONONETOMANY_HPP
#define RELATIONONETOMANY_HPP

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <queue>

#include "godoftypes.hpp"
#include "seque.hpp"

namespace hidden
{
    using lst = seque<size_t>;
    struct o2m
    {
        seque<seque<size_t>> lnods;
        size_t nelem{0};
        size_t maxnodenumber{0};
    };
    PFR_FUNCTIONS_FOR(o2m)
    void setnelem(o2m &rel, size_t nelem);
    size_t appendelement(o2m &rel, const seque<size_t> &nodes);
    void transpose(const o2m &rel, o2m &relt);
    void multiplication(const o2m &rela, const o2m &relb, o2m &relc);
    void addition(const o2m &rela, const o2m &relb, o2m &relc);
    void intersection(const o2m &a, const o2m &b, o2m &c);
    void subtraction(const o2m &rela, const o2m &relb, o2m &relc);
    seque<size_t> toporder(const o2m &rel);
    seque<size_t> lexiorder(const o2m &rel);
    void indicesfromorder(const o2m &rel, const seque<size_t> &elemOrder, seque<size_t> &oldFromNew,
                          seque<size_t> &newFromOld);
    void compresselements(o2m &rel, const seque<size_t> &oldelementfromnew);
    void permutenodes(o2m &rel, const seque<size_t> &newnodefromold);

} // namespace hidden

#endif
