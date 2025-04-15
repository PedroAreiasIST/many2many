#ifndef RELMANYTOMANY_HPP
#define RELMANYTOMANY_HPP

#include <cstddef>

#include "godoftypes.hpp"
#include "o2m.hpp"
#include "seque.hpp"

struct m2m {
  o2m nfrome;
  o2m efromn;
  seque<seque<int>> nodeloc;
  seque<seque<int>> elementloc;
  bool isupdated{false};
};

PFR_FUNCTIONS_FOR(m2m)

int appendelement(m2m &rel, seque<int> const &nodes);

void setsyncronized(m2m &rel);

seque<int> getelementscontainingnodes(m2m const &rel, seque<int> const &nodes);

seque<int> getelementsdefinedbythesenodes(m2m const &rel,
                                          seque<int> const &nodes);

seque<int> getelementneighbours(m2m const &rel, int element);

seque<int> getnodeneighbours(m2m const &rel, int node);

seque<int> getorder(m2m const &rel);

seque<int> gettoporder(m2m const &rel);

void compresselements(m2m &rel, seque<int> const &oldelementfromnew);

void permutenodes(m2m &rel, seque<int> const &newnodefromold);

void getelementstoelements(m2m const &rel, m2m &elementstoelements);

void getnodestonodes(m2m const &rel, m2m &nodestonodes);

seque<seque<int>> getcliques(m2m const &rel);

#endif
