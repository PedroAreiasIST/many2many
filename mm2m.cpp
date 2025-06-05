#include "mm2m.hpp"

#include "o2m.hpp"
#include "basics.hpp"
#include <algorithm>
#include <cassert>
#include <set>
#include <stack>
#include <utility>

m2m const &mm2m::operator()(int elementtype, int nodetype) const
{
    return m[elementtype][nodetype];
}

m2m &mm2m::operator()(int elementtype, int nodetype)
{
    return m[elementtype][nodetype];
}

int mm2m::nnodes(int elementType, int element, int nodeType) const
{
    assert(elementType >= 0 && elementType < ntypes);
    assert(nodeType >= 0 && nodeType < ntypes);
    assert(element >= 0 &&
        element < getsize(operator()(elementType, nodeType).nfrome.lnods));
    return getsize(operator()(elementType, nodeType).nfrome.lnods[element]);
}

int mm2m::nelems(int nodeType, int node, int elementType) const
{
    assert(nodeType >= 0 && nodeType < ntypes);
    assert(elementType >= 0 && elementType < ntypes);
    if (node < 0 ||
        node >= getsize(operator()(elementType, nodeType).efromn.lnods))
    {
        return 0;
    } else
        return getsize(operator()(elementType, nodeType).efromn.lnods[node]);
}

int mm2m::nelems(int elementtype) const
{
    return m[elementtype][elementtype].nfrome.nelems();
}

int mm2m::nactiveelements(int elementtype) const
{
    int nactive = 0;
    for (int element = 0; element < m[elementtype][elementtype].nfrome.nelems();
         ++element)
    {
        if (getsize(m[elementtype][elementtype].nfrome[element]) != 0)
            nactive++;
    }
    return nactive;
}

void marktoerase(mm2m &m, int nodeType, int node)
{
    append(m.listofmarked, std::make_pair(nodeType, node));
}

void marktoeraserepeated(mm2m &m, int elementtype, int nodetype)
{
    auto mm = m(elementtype, nodetype);
    auto order = getorder(mm);
    auto dupindices = getindicesofduplicates(mm.nfrome.lnods, order);
    for (int i = 0; i < getsize(dupindices); ++i)
        marktoerase(m, elementtype, dupindices[i]);
}

seque<std::pair<int, int> > getallelements(mm2m const &m, int nodetype,
                                           int node)
{
    seque<std::pair<int, int> > ret;
    if (node < 0 || node >= m(nodetype, nodetype).nfrome.nelem)
    {
        return ret;
    }
    int totalElements = 0;
    for (int elementType = 0; elementType < m.ntypes; ++elementType)
    {
        if (elementType != nodetype)
            totalElements += m.nelems(nodetype, node, elementType);
    }
    setsize(ret, totalElements);
    int pos = 0;
    for (int elementType = 0; elementType < m.ntypes; ++elementType)
    {
        if (elementType != nodetype)
        {
            int numElems = m.nelems(nodetype, node, elementType);
            for (int localelem = 0; localelem < numElems; ++localelem)
            {
                ret[pos++] = std::make_pair(
                    elementType,
                    m(elementType, nodetype).efromn.lnods[node][localelem]);
            }
        }
    }
    setorderedandunique(ret);
    return ret;
}

seque<std::pair<int, int> > getallelements(mm2m const &m, int nodetype)
{
    seque<std::pair<int, int> > ret;
    for (int node = 0; node < m(nodetype, nodetype).nfrome.nelem; ++node)
    {
        ret = getunion(ret, getallelements(m, nodetype, node));
    }
    setorderedandunique(ret);
    return ret;
}

seque<std::pair<int, int> > getallnodes(mm2m const &m, int elementType,
                                        int elementNumber)
{
    seque<std::pair<int, int> > ret;
    if (elementNumber < 0 || elementNumber >= m(elementType, elementType).nfrome.nelem)
    {
        return ret;
    }
    int totalNodes = 0;
    for (int nodeType = 0; nodeType < m.ntypes; ++nodeType)
    {
        totalNodes += m.nnodes(elementType, elementNumber, nodeType);
    }
    setsize(ret, totalNodes);
    int pos = 0;
    for (int nodeType = 0; nodeType < m.ntypes; ++nodeType)
    {
        int numberOfNodes = m.nnodes(elementType, elementNumber, nodeType);
        for (int localnode = 0; localnode < numberOfNodes; ++localnode)
        {
            ret[pos++] = std::make_pair(
                nodeType,
                m(elementType, nodeType).nfrome.lnods[elementNumber][localnode]);
        }
    }
    setorderedandunique(ret);
    return ret;
}

seque<std::pair<int, int> > getallnodes(mm2m const &m, int elementtype)
{
    seque<std::pair<int, int> > ret;
    for (int element = 0; element < m(elementtype, element).nfrome.nelem;
         ++element)
    {
        ret = getunion(ret, getallnodes(m, elementtype, element));
    }
    setorderedandunique(ret);
    return ret;
}

seque<std::pair<int, int> >
hidden::depthfirstsearchfromanode(mm2m const &m,
                                  std::pair<int, int> const &node)
{
    using P = std::pair<int, int>;
    seque<P> ret;
    std::set<P> visited;
    std::stack<P> stack;
    stack.push(node);
    while (!stack.empty())
    {
        auto current = stack.top();
        stack.pop();
        if (visited.find(current) == visited.end())
        {
            visited.insert(current);
            append(ret, current);
            seque<P> elements = getallelements(m, current.first, current.second);
            for (int i = 0; i < getsize(elements); ++i)
            {
                if (visited.find(elements[i]) == visited.end())
                    stack.push(elements[i]);
            }
        }
    }
    setorderedandunique(ret);
    return ret;
}

void setnumberoftypes(mm2m &m, int ntypes)
{
    m.ntypes = ntypes;
    setsize(m.m, ntypes);
    for (int type = 0; type < ntypes; ++type)
    {
        setsize(m.m[type], ntypes);
    }
}

int appendelement(mm2m &m, int elementType, int nodeType,
                  seque<int> const &nodes)
{
    int newelement = appendelement(m(elementType, nodeType), nodes);
    return newelement;
}

void setnumberofelements(mm2m &m, int elementtype, int nelem)
{
    if (nelem < m(elementtype, elementtype).nfrome.nelem)
    {
        throw std::runtime_error("New number of elements is less than the current number of elements.");
    }
    for (int nodetype = 0; nodetype < m.ntypes; ++nodetype)
    {
        setnumberofelements(m(elementtype, nodetype), nelem);
    }
}

void compress(mm2m &m)
{
    setorderedandunique(m.listofmarked);
    int markedSize = getsize(m.listofmarked);
    for (int counter = 0; counter < markedSize; ++counter)
    {
        seque<std::pair<int, int> > dfsResult =
                hidden::depthfirstsearchfromanode(m, m.listofmarked[counter]);
        for (int i = 0; i < getsize(dfsResult); ++i)
        {
            append(m.listofmarked, dfsResult[i]);
        }
    }
    setorderedandunique(m.listofmarked);

    seque<seque<int> > nodes(m.ntypes);
    for (int counter = 0; counter < getsize(m.listofmarked); ++counter)
    {
        auto pair = m.listofmarked[counter];
        int type = pair.first;
        int node = pair.second;
        append(nodes[type], node);
    }
    for (int type = 0; type < m.ntypes; ++type)
    {
        int nnmax = 0;
        for (int otherType = 0; otherType < m.ntypes; ++otherType)
        {
            nnmax = std::max(nnmax, m(type, otherType).nfrome.nelems());
            nnmax = std::max(nnmax, m(type, otherType).efromn.maxnode + 1);
            nnmax = std::max(nnmax, m(otherType, type).efromn.nelems());
            nnmax = std::max(nnmax, m(otherType, type).nfrome.maxnode + 1);
        }
        setnumberofelements(m(type, type), nnmax);
        for (int i = 0; i < nnmax; ++i)
        {
            appendelement(m(type, type), {i});
        }
    }
    for (int type = 0; type < m.ntypes; ++type)
    {
        for (int lelement = 0; lelement < getsize(nodes[type]); ++lelement)
        {
            auto element = nodes[type][lelement];
            for (int otype = 0; otype < m.ntypes; ++otype)
            {
                erase(m(type, otype).nfrome[element]);
            }
        }
        for (int otype = 0; otype < m.ntypes; ++otype)
        {
            synchronize(m(type, otype));
        }
    }
}

seque<int> gettypetoporder(mm2m const &m)
{
    o2m typeDeps;
    setsize(typeDeps, m.ntypes);
    for (int elementType = 0; elementType < m.ntypes; ++elementType)
    {
        for (int nodeType = 0; nodeType < m.ntypes; ++nodeType)
        {
            if (nodeType != elementType)
            {
                if (m(elementType, nodeType).nfrome.nelem != 0)
                {
                    append(typeDeps.lnods[elementType], nodeType);
                }
            }
        }
    }
    return gettoporder(typeDeps);
}

seque<int> getelementsfromnodes(mm2m &matrix, int elementType, int nodeType,
                                seque<int> const &nodes)
{
    return getelementsfromnodes(matrix(elementType, nodeType), nodes);
}

seque<int> getelementswithnodes(mm2m &matrix, int elementtype, int nodestype,
                                seque<int> const &nodes)
{
    return getelementswithnodes(matrix(elementtype, nodestype), nodes);
}
