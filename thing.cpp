#include "thing.hpp"
void settypenumber(thing &e, size_t type) { e.type = type; }
void settypenumber(seque<thing> &es, size_t type)
{
    for (auto e: es)
    {
        settypenumber(e, type);
    }
}
void appendnodesofonetype(thing &e, size_t nodetype, seque<size_t> const &nodes)
{
    append(e.typeandnodes, std::make_pair(nodetype, nodes));
}
void appendbuilder(thing &eparent, thing &echildren, seque<std::pair<size_t, seque<size_t>>> const &typeandlocalnodes)
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
void insertathing(mm2m &m, thing const &e)
{
    for (size_t localtype = 0; localtype < getsize(e.typeandnodes); ++localtype)
    {
        appendelement(m, e.type, e.typeandnodes[localtype].first, e.typeandnodes[localtype].second);
    }
}
