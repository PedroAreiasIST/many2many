#ifndef RELATIONONETOMANY_HPP
#define RELATIONONETOMANY_HPP

#include "basics.hpp"
#include <cassert>
#include <cstddef>
#include <queue>

#include "seque.hpp"

namespace hidden
{
    using lst = seque<int>;
}

struct o2m
{
    seque<seque<int> > lnods{{}};
    int nelem{0};
    int maxnodenumber{0};
    // Access operators
    seque<int> &operator[](int element) { return lnods[element]; }
    seque<int> const &operator[](int element) const { return lnods[element]; }
    // Utility methods
    int size() const { return nelem; }
    int size(int element) const { return lnods[element].size; }
};

PFR_FUNCTIONS_FOR(o2m)

void setnumberofelements(o2m &rel, int nelem);

void setnodesforelement(o2m &rel, int element, seque<int> &&nodes);

void setnodesforelement(o2m &rel, int element, seque<int> const &nodes);

int appendelement(o2m &rel, const seque<int> &nodes);

o2m transpose(const o2m &rel);

void multiplication(const o2m &rela, const o2m &relb, o2m &relc);

void multiplication(const o2m &rela, const seque<int> &relb, o2m &relc);

o2m operator*(const o2m &rela, const o2m &relb);

o2m operator*(const o2m &rela, const seque<int> &vec);

void addition(const o2m &rela, const o2m &relb, o2m &relc);

o2m operator+(const o2m &rela, const o2m &rel);

void intersection(const o2m &a, const o2m &b, o2m &c);

o2m operator||(const o2m &a, const o2m &b);

o2m operator&&(const o2m &a, const o2m &b);

void subtraction(const o2m &rela, const o2m &relb, o2m &relc);

o2m operator-(const o2m &rela, const o2m &relb);

seque<int> toporder(const o2m &rel);

seque<int> lexiorder(const o2m &rel);

void indicesfromorder(const o2m &rel, const seque<int> &elemOrder,
                      seque<int> &oldFromNew, seque<int> &newFromOld);

void compresselements(o2m &rel, const seque<int> &oldelementfromnew);

void permutenodes(o2m &rel, const seque<int> &newnodefromold);

seque<seque<int> > getnodepositions(o2m const &nodesfromelement,
                                    o2m const &elementsfromnode);

seque<seque<int> > getelementpositions(o2m const &nodesfromelement,
                                       o2m const &elementsfromnode);

o2m getcliqueaddressing(o2m const &nodesfromelement,
                        o2m const &elementsfromnode);

o2m convertfromlist(const seque<int> &other);
#endif
