#include "entity.hpp"
void settype(entity &e, size_t type) { e.type = type; }
void appendnodesofonetype(entity &e, size_t nodetype, sek<size_t> const &nodes)
{
    append(e.typeandnodes, std::make_pair(nodetype, nodes));
}

void insertentity(relationmatrix &m, entity const &e)
{
    for (size_t types = 0; types < getsize(e.typeandnodes); ++types)
    {
        appendelement(m, e.type, e.typeandnodes[types].first, e.typeandnodes[types].second);
    }
}
