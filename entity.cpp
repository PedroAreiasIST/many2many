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
    for (size_t localnodetype = 0; localnodetype < getsize(typeandlocalnodes); ++localnodetype)
    {
        size_t type = typeandlocalnodes[localnodetype].first;
        for (size_t localtype = 0; localtype < getsize(eparent.typeandnodes); ++localtype)
        {
            if (eparent.typeandnodes[localtype].first == type)
            {
                appendnodesofonetype(echildren, type,
                                     eparent.typeandnodes[localtype].second(typeandlocalnodes[localnodetype].second));
            }
        }
    }
}
void insertentity(relationmatrix &m, entity const &e)
{
    for (size_t localtype = 0; localtype < getsize(e.typeandnodes); ++localtype)
    {
        appendelement(m, e.type, e.typeandnodes[localtype].first, e.typeandnodes[localtype].second);
    }
}
