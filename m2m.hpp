#ifndef RELMANYTOMANY_HPP
#define RELMANYTOMANY_HPP

#include <cstddef>

#include "godoftypes.hpp"
#include "o2m.hpp"
#include "seque.hpp"


struct m2m
{
    o2m e2n;
    o2m n2e;
    seque<seque<int> > nodelocation;
    seque<seque<int> > elementlocation;

    bool isupdated{false};
};

PFR_FUNCTIONS_FOR(m2m)

void setsize(m2m &rel, int nelem);

void setnodesforelement(m2m &rel, int element, seque<int> const &nodes);

int appendelement(m2m &rel, seque<int> const &nodes);

void setallpointers(m2m &rel);

seque<int> getelementswithnodes(m2m const &rel, seque<int> const &nodes);

seque<int> getelementsfromnodes(m2m const &rel, seque<int> const &nodes);

seque<int> getelementneighbours(m2m const &rel, int element);

seque<int> getnodeneighbours(m2m const &rel, int node);

seque<int> lexiorder(m2m const &rel);

seque<int> toporder(m2m const &rel);

void compresselements(m2m &rel, seque<int> const &oldelementfromnew);

void permutenodes(m2m &rel, seque<int> const &newnodefromold);

void getelementstoelements(m2m const &rel, m2m &elementstoelements);

void getnodestonodes(m2m const &rel, m2m &nodestonodes);

#endif
