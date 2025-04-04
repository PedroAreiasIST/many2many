#ifndef RELATIONMANAGER_HPP
#define RELATIONMANAGER_HPP
#include "mm2m.hpp"
#include "seque.hpp"
#include "symmetries.hpp"

struct thing
{
    using NodeType = size_t;
    using ListofNodes = seque<size_t>;
    size_t type;
    seque<std::pair<NodeType, ListofNodes>> typeandnodes;
};
PFR_FUNCTIONS_FOR(thing)

void settypenumber(thing &e, size_t type);

void settypenumber(seque<thing> &es, size_t type);

void appendnodesofonetype(thing &e, size_t nodetype, seque<size_t> const &nodes);

void appendbuilder(thing &eparent, thing &echildren, seque<std::pair<size_t, seque<size_t>>> const &typeandlocalnodes);

void insertathing(mm2m &m, thing const &e);

#endif // RELATIONMANAGER_HPP
