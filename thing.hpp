#ifndef RELATIONMANAGER_HPP
#define RELATIONMANAGER_HPP
#include "mm2m.hpp"
#include "seque.hpp"
#include "symmetries.hpp"

struct thing
{
    int type;
    seque<std::pair<int, seque<int> > > typeandnodes;
};

PFR_FUNCTIONS_FOR(thing)

void settypenumber(thing &e, int type);

void settypenumber(seque<thing> &es, int type);

void appendnodesofonetype(thing &e, int nodetype, seque<int> const &nodes);

void appendbuilder(thing &eparent, thing &echildren, seque<std::pair<int, seque<int> > > const &typeandlocalnodes);

void uploadathing(mm2m &m, thing const &e);

#endif // RELATIONMANAGER_HPP
