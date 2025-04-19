#ifndef RELMANYTOMANY_HPP
#define RELMANYTOMANY_HPP

#include "o2m.hpp"
#include "seque.hpp"
#include "superstruct.hpp"
#include <cstddef>

struct m2m
{
    o2m nfrome;
    o2m efromn;
    seque<seque<int> > nodeloc;
    seque<seque<int> > elementloc;
    bool isupdated{false};
};

PFR_FUNCTIONS_FOR(m2m)

int appendelement(m2m &rel, seque<int> const &nodes);

void setnumberofelements(m2m &rel, int nelem);

void setnodesforelement(m2m &rel, int element, seque<int> const &nodes);

void setsyncronized(m2m &rel);

seque<int> getelementswithnodes(m2m const &rel, seque<int> const &nodes);

seque<int> getelementsfromnodes(m2m const &rel, seque<int> const &nodes);

seque<int> getelementneighbours(m2m const &rel, int element);

seque<int> getnodeneighbours(m2m const &rel, int node);

seque<int> getorder(m2m const &rel);

seque<int> gettoporder(m2m const &rel);

void compresselements(m2m &rel, seque<int> const &oldelementfromnew);

void permutenodes(m2m &rel, seque<int> const &newnodefromold);

m2m getelementstoelements(m2m const &rel);

m2m getnodestonodes(m2m const &rel);

seque<seque<int> > getcliques(m2m const &rel);

#endif
