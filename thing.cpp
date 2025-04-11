#include "thing.hpp"
void settypenumber(thing &e, int type) { e.type = type; }
void settypenumber(seque<thing> &es, int type)
{
    for (auto e: es)
    {
        settypenumber(e, type);
    }
}
void appendnodesofonetype(thing &e, int nodetype, seque<int> const &nodes)
{
    append(e.typeandnodes, std::make_pair(nodetype, nodes));
}

void appendbuilder(thing &eparent, thing &echildren, seque<std::pair<int, seque<int>>> const &typeandlocalnodes)
{
    for (int localnodetype = 0; localnodetype < getsize(typeandlocalnodes); ++localnodetype)
    {
        int type = typeandlocalnodes[localnodetype].first;
        for (int localtype = 0; localtype < getsize(eparent.typeandnodes); ++localtype)
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
    for (int localtype = 0; localtype < getsize(e.typeandnodes); ++localtype)
    {
        appendelement(m, e.type, e.typeandnodes[localtype].first, e.typeandnodes[localtype].second);
    }
}
