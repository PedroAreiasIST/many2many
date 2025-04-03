#ifndef RELATIONONETOMANY_HPP
#define RELATIONONETOMANY_HPP

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <queue>
#include "seque.hpp"

namespace hidden
{
    using lst = seque<size_t>;
    using lst2 = seque<lst>;
    struct one2many
    {
        hidden::lst2 lnods;
        size_t nelem{0};
        size_t maxnodenumber{0};
    };
    PFR_FUNCTIONS_FOR(one2many)
    void setnelem(one2many &rel, size_t nelem);
    size_t appendelement(one2many &rel, const hidden::lst &nodes);
    void transpose(const one2many &rel, one2many &relt);
    void multiplication(const one2many &rela, const one2many &relb, one2many &relc);
    void addition(const one2many &rela, const one2many &relb, one2many &relc);
    void intersection(const one2many &a, const one2many &b, one2many &c);
    void difference(const one2many &rela, const one2many &relb, one2many &relc);
    void toporder(const one2many &rel, hidden::lst &order);
    void lexiorder(const one2many &rel, hidden::lst &orderofelements);
    void indicesfromorder(const one2many &rel, const hidden::lst &elemOrder, hidden::lst &oldFromNew,
                          hidden::lst &newFromOld);
    void compresselements(one2many &rel, const hidden::lst &oldelementfromnew);
    void permutenodes(one2many &rel, const hidden::lst &newnodefromold);

} // namespace hidden

#endif
