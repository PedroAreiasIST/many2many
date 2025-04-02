#ifndef RELATIONMANAGER_HPP
#define RELATIONMANAGER_HPP
#include "relationmatrix.hpp"
#include "sek.hpp"
#include "symmetries.hpp"

struct thing
{
    using NodeType = size_t;
    using ListofNodes = sek<size_t>;
    size_t type;
    sek<std::pair<NodeType, ListofNodes>> typeandnodes;
};
PFR_FUNCTIONS_FOR(thing)

void settype(thing &e, size_t type);

void settype(sek<thing> &es, size_t type);

void appendnodesofonetype(thing &e, size_t nodetype, sek<size_t> const &nodes);

void appendbuilder(thing &eparent, thing &echildren, sek<std::pair<size_t, sek<size_t>>> const &typeandlocalnodes);

void insertentity(relationmatrix &m, thing const &e);

#endif // RELATIONMANAGER_HPP
