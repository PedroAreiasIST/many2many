#ifndef RELATIONMANAGER_HPP
#define RELATIONMANAGER_HPP
#include "relationmatrix.hpp"
#include "sequence.hpp"
#include "symmetries.hpp"

struct thing
{
    using NodeType = size_t;
    using ListofNodes = sequence<size_t>;
    size_t type;
    sequence<std::pair<NodeType, ListofNodes>> typeandnodes;
};
PFR_FUNCTIONS_FOR(thing)

void settype(thing &e, size_t type);

void settype(sequence<thing> &es, size_t type);

void appendnodesofonetype(thing &e, size_t nodetype, sequence<size_t> const &nodes);

void appendbuilder(thing &eparent, thing &echildren,
                   sequence<std::pair<size_t, sequence<size_t>>> const &typeandlocalnodes);

void insertathing(relationmatrix &m, thing const &e);

#endif // RELATIONMANAGER_HPP
