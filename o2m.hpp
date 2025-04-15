#ifndef RELATIONONETOMANY_HPP
#define RELATIONONETOMANY_HPP
#include "basics.hpp"
#include "seque.hpp"

namespace hidden {
using lst = seque<int>;
}

struct o2m {
  seque<seque<int>> lnods{{}};
  int nelem{0};
  int maxnode{0};
  // Access operators
  seque<int> &operator[](int element) { return lnods[element]; }
  seque<int> const &operator[](int element) const { return lnods[element]; }
  // Utility methods
  int size() const { return nelem; }
  int size(int element) const { return lnods[element].size; }
};

PFR_FUNCTIONS_FOR(o2m)

void setsize(o2m &rel, int nelem);

void setsizes(o2m &rel, seque<int> const &sizes);

void setnodesforelement(o2m &rel, int element, seque<int> &&nodes);

void setnodesforelement(o2m &rel, int element, seque<int> const &nodes);

int appendelement(o2m &rel, const seque<int> &nodes);

o2m &operator<<(o2m &rel, std::initializer_list<int> nodes);

o2m Tr(const o2m &rel);

o2m operator*(const o2m &rela, const o2m &relb);

o2m operator*(const o2m &rela, const seque<int> &vec);

o2m operator+(const o2m &rela, const o2m &rel);

o2m operator||(const o2m &a, const o2m &b);

o2m operator&&(const o2m &a, const o2m &b);

o2m operator-(const o2m &rela, const o2m &relb);

seque<int> gettoporder(const o2m &rel);

seque<int> getorder(const o2m &rel);

namespace hidden {
void compresselements(o2m &rel, const seque<int> &oldelementfromnew);

void permutenodes(o2m &rel, const seque<int> &newnodefromold);

seque<seque<int>> getnodepositions(o2m const &nodesfromelement,
                                   o2m const &elementsfromnode);

seque<seque<int>> getelementpositions(o2m const &nodesfromelement,
                                      o2m const &elementsfromnode);
} // namespace hidden

o2m geto2mfromsequence(const seque<int> &other);

seque<seque<int>> getcliques(const o2m &nodesfromelement,
                             const o2m &elementsfromnode);

#endif
