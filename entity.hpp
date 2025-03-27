#ifndef RELATIONMANAGER_HPP
#define RELATIONMANAGER_HPP
#include "relationmatrix.hpp"
#include "sek.hpp"
#include "symmetries.hpp"

struct entity
{
    using NodeType = size_t;
    using ListofNodes = sek<size_t>;
    size_t type;
    sek<std::pair<NodeType, ListofNodes>> typeandnodes;
};
PFR_FUNCTIONS_FOR(entity)

void settype(entity &e, size_t type);

void settype(sek<entity> &es, size_t type);

void appendnodesofonetype(entity &e, size_t nodetype, sek<size_t> const &nodes);

void appendbuilder(entity &eparent, entity &echildren, sek<std::pair<size_t, sek<size_t>>> const &typeandlocalnodes);

void insertentity(relationmatrix &m, entity const &e);

#endif // RELATIONMANAGER_HPP
