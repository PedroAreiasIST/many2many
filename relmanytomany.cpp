// Created by pedro on 9/18/24.
#include "relmanytomany.hpp"

void setfromonetomany(relmanytomany &rel)
{
    transpose(rel.nodesfromelement, rel.elementsfromnode);
    setsize(rel.locn, rel.elementsfromnode.nelems);
    for (size_t i = 0; i < getsize(rel.locn); ++i)
    {
        setsize(rel.locn[i], getsize(rel.nodesfromelement.lnods[i]));
    }
    lst nen{rel.elementsfromnode.nelems};
    for (size_t e = 0, nE = getsize(rel.nodesfromelement.lnods); e < nE; ++e)
    {
        for (size_t nl = 0, nNL = getsize(rel.nodesfromelement.lnods[e]); nl < nNL; ++nl)
        {
            size_t node = rel.nodesfromelement.lnods[e][nl];
            rel.locn[node][nen[node]] = nl;
            nen[node]++;
        }
    }
}

lst getelementsfromnodes(relmanytomany const &rel, lst const &nodes)
{
    lst elems;
    if (getsize(nodes) > 0)
    {
        elems = rel.elementsfromnode.lnods[nodes[0]];
        for (size_t i = 1, n = getsize(nodes); i < n; ++i)
            elems = getintersection(elems, rel.elementsfromnode.lnods[nodes[i]]);
    }
    return elems;
}

lst getneighbours(relmanytomany const &rel, size_t element)
{
    lst ret;
    size_t count = 0;
    for (size_t nl = 0, nNL = getsize(rel.nodesfromelement.lnods[element]); nl < nNL; ++nl)
    {
        size_t node = rel.nodesfromelement.lnods[element][nl];
        for (size_t el = 0, nEL = getsize(rel.elementsfromnode.lnods[node]); el < nEL; ++el)
            if (rel.elementsfromnode.lnods[node][el] != element)
                count++;
    }
    setsize(ret, count);
    count = 0;
    for (size_t nl = 0, nNL = getsize(rel.nodesfromelement.lnods[element]); nl < nNL; ++nl)
    {
        size_t node = rel.nodesfromelement.lnods[element][nl];
        for (size_t el = 0, nEL = getsize(rel.elementsfromnode.lnods[node]); el < nEL; ++el)
        {
            size_t otherElem = rel.elementsfromnode.lnods[node][el];
            if (otherElem != element)
                ret[count++] = otherElem;
        }
    }
    setordered(ret);
    setunique(ret);
    return ret;
}

void indicesfromorder(relmanytomany const &rel, const lst &elementorder, lst &oldfromnew, lst &newfromold)
{
    indicesfromorder(rel.nodesfromelement, elementorder, oldfromnew, newfromold);
}

void compresselements(relmanytomany &rel, lst const &oldelementfromnew)
{
    compresselements(rel.nodesfromelement, oldelementfromnew);
    setfromonetomany(rel);
}

void compressnodes(relmanytomany &rel, lst const &newnodefromold)
{
    compressnodes(rel.nodesfromelement, newnodefromold);
    setfromonetomany(rel);
}

void lexiorder(relmanytomany const &rel, lst &order) { toporder(rel.nodesfromelement, order); }

void toporder(relmanytomany const &rel, bool doTranspose, lst &order)
{
    if (!doTranspose)
        toporder(rel.nodesfromelement, order);
    else
        toporder(rel.elementsfromnode, order);
}
