#include "entity.hpp"
void settype(entity &e, size_t type) { e.type = type; }
void settype(sek<entity> &es, size_t type)
{
    for (auto e: es)
    {
        settype(e, type);
    }
}
void appendnodesofonetype(entity &e, size_t nodetype, sek<size_t> const &nodes)
{
    append(e.typeandnodes, std::make_pair(nodetype, nodes));
}
void appendbuilder(entity &eparent, entity &echildren, sek<std::pair<size_t, sek<size_t>>> const &typeandlocalnodes)
{
    for (size_t localnodetypes = 0; localnodetypes < getsize(typeandlocalnodes); ++localnodetypes)
    {
        size_t type = typeandlocalnodes[localnodetypes].first;
        for (size_t localtype = 0; localtype < getsize(eparent.typeandnodes); ++localtype)
        {
            if (eparent.typeandnodes[localtype].first == type)
            {
                appendnodesofonetype(echildren, type,
                                     eparent.typeandnodes[localtype].second(typeandlocalnodes[localnodetypes].second));
            }
        }
    }
}
void insertentity(relationmatrix &m, entity const &e)
{
    for (size_t types = 0; types < getsize(e.typeandnodes); ++types)
    {
        appendelement(m, e.type, e.typeandnodes[types].first, e.typeandnodes[types].second);
    }
}
