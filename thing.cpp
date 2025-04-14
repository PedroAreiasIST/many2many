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

thing getthingfrombuilder(thing &eparent, int childtype, seque<std::pair<int, seque<int> > > const &typeandlocalnodesinparent)
{
    thing result;
    settypenumber(result, childtype);
    for (int localnodetype = 0; localnodetype < getsize(typeandlocalnodesinparent); ++localnodetype)
    {
        int nodetype = typeandlocalnodesinparent[localnodetype].first;
        seque<int> localnodes = typeandlocalnodesinparent[localnodetype].second;
        for (int localtype = 0; localtype < getsize(eparent.typeandnodes); ++localtype)
        {
            if (eparent.typeandnodes[localtype].first == nodetype)
            {
                appendnodesofonetype(result, nodetype, eparent.typeandnodes[localtype].second(localnodes));
            }
        }
    }
    return result;
}

void uploadathing(mm2m &m, thing const &e)
{
    for (int localtype = 0; localtype < getsize(e.typeandnodes); ++localtype)
    {
        appendelement(m, e.type, e.typeandnodes[localtype].first, e.typeandnodes[localtype].second);
    }
}

void uploadthings(mm2m &m, seque<thing> const &es)
{
    for (auto e: es)
    {
        uploadathing(m, e);
    }
}
