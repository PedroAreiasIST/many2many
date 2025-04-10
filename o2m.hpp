#ifndef RELATIONONETOMANY_HPP
#define RELATIONONETOMANY_HPP

#include "basics.hpp"
#include <cassert>
#include <cstddef>
#include <queue>

#include "seque.hpp"

namespace hidden {
using lst = seque<size_t>;
}

struct o2m {
  seque<seque<size_t>> lnods{{}};
  size_t nelem{0};
  size_t maxnodenumber{0};
  // Access operators
  seque<size_t> &operator[](size_t element) { return lnods[element]; }
  seque<size_t> const &operator[](size_t element) const {
    return lnods[element];
  }
  // Utility methods
  size_t size() const { return nelem; }
  size_t elementsize(size_t element) const { return lnods[element].size; }
};
PFR_FUNCTIONS_FOR(o2m)

void setnumberofelements(o2m &rel, size_t nelem);
void setnodesforelement(o2m &rel, size_t element, seque<size_t> &&nodes);
void setnodesforelement(o2m &rel, size_t element, seque<size_t> const &nodes);
size_t appendelement(o2m &rel, const seque<size_t> &nodes);
o2m transpose(const o2m &rel);

void multiplication(const o2m &rela, const o2m &relb, o2m &relc);
void multiplication(const o2m &rela, const seque<size_t> &relb, o2m &relc);
o2m operator*(const o2m &rela, const o2m &relb);
o2m operator*(const o2m &rela, const seque<size_t> &vec);
void addition(const o2m &rela, const o2m &relb, o2m &relc);
o2m operator+(const o2m &rela, const o2m &rel);
void intersection(const o2m &a, const o2m &b, o2m &c);
o2m operator||(const o2m &a, const o2m &b);
o2m operator&&(const o2m &a, const o2m &b);
void subtraction(const o2m &rela, const o2m &relb, o2m &relc);
o2m operator-(const o2m &rela, const o2m &relb);
seque<size_t> toporder(const o2m &rel);
seque<size_t> lexiorder(const o2m &rel);
void indicesfromorder(const o2m &rel, const seque<size_t> &elemOrder,
                      seque<size_t> &oldFromNew, seque<size_t> &newFromOld);
void compresselements(o2m &rel, const seque<size_t> &oldelementfromnew);
void permutenodes(o2m &rel, const seque<size_t> &newnodefromold);
seque<seque<size_t>> getnodelocation(o2m const &nodesfromelement,
                                     o2m const &elementsfromnode);

o2m getcliqueaddressing(o2m const &nodesfromelement,
                        o2m const &elementsfromnode);

o2m convertfromlist(const seque<size_t> &other);
#endif
