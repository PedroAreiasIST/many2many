#ifndef RELATIONMANAGER_HPP
#define RELATIONMANAGER_HPP
#include "mm2m.hpp"
#include "seque.hpp"
#include "symmetries.hpp"

struct

    struct thing {
  int type;
  seque<std::pair<int, seque<int>>> typeandnodes;
};

PFR_FUNCTIONS_FOR(thing)

void settypenumber(thing &e, int type);

void settypenumber(seque<thing> &es, int type);

void appendnodesofonetype(thing &e, int nodetype, seque<int> const &nodes);

thing getthingfrombuilder(
    thing &eparent, int childtype,
    seque<std::pair<int, seque<int>>> const &typeandlocalnodesinparent);

void uploadathing(mm2m &m, thing const &e);

void uploadthings(mm2m &m, seque<thing> const &es);

#endif // RELATIONMANAGER_HPP
