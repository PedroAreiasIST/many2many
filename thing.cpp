#include "thing.hpp"
void settype(thing &e, size_t type) { e.type = type; }
void settype(sequence<thing> &es, size_t type)
{
    for (auto e: es)
    {
        settype(e, type);
    }
}
void appendnodesofonetype(thing &e, size_t nodetype, sequence<size_t> const &nodes)
{
    append(e.typeandnodes, std::make_pair(nodetype, nodes));
}
void appendbuilder(thing &eparent, thing &echildren,
                   sequence<std::pair<size_t, sequence<size_t>>> const &typeandlocalnodes)
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
void insertathing(relationmatrix &m, thing const &e)
{
    for (size_t localtype = 0; localtype < getsize(e.typeandnodes); ++localtype)
    {
        appendelement(m, e.type, e.typeandnodes[localtype].first, e.typeandnodes[localtype].second);
    }
}
