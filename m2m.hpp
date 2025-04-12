#ifndef RELMANYTOMANY_HPP
#define RELMANYTOMANY_HPP

#include <cstddef>

#include "godoftypes.hpp"
#include "o2m.hpp"
#include "seque.hpp"

namespace hidden
{
    using lst = seque<int>;
    using lst2 = seque<lst>;
} // namespace hidden

struct m2m
{
    o2m nodesfromelement;
    o2m elementsfromnode;
    seque<seque<int>> nodelocation;
    int nnodes(int element);
    int nelems(int node);
    bool isupdated{false};
};
PFR_FUNCTIONS_FOR(m2m)
void setsize(m2m &rel, int nelem);
void setnodesforelement(m2m &rel, int element, seque<int> const &nodes);
int appendelement(m2m &rel, seque<int> const &nodes);
void multiplication(const m2m &rela, bool transposea, const m2m &relb, bool transposeb, m2m &relc);
void addition(const m2m &rela, bool transposea, const m2m &relb, bool transposeb, m2m &relc);
void intersection(const m2m &rela, bool transposea, const m2m &relb, bool transposeb, m2m &relc);
void subtraction(const m2m &rela, bool transposea, const m2m &relb, bool transposeb, m2m &relc);
void setallpointers(m2m &rel);
seque<int> getelementswithnodes(m2m const &rel, seque<int> const &nodes);
seque<int> getelementsfromnodes(m2m const &rel, seque<int> const &nodes);
seque<int> getelementneighbours(m2m const &rel, int element);
seque<int> getnodeneighbours(m2m const &rel, int node);
seque<int> lexiorder(m2m const &rel);
seque<int> toporder(m2m const &rel);
void indicesfromorder(m2m const &rel, const seque<int> &elementorder, seque<int> &oldfromnew,
                      seque<int> &newfromold);
void compresselements(m2m &rel, seque<int> const &oldelementfromnew);
void permutenodes(m2m &rel, seque<int> const &newnodefromold);
void getelementstoelements(m2m const &rel, m2m &elementstoelements);
void getnodestonodes(m2m const &rel, m2m &nodestonodes);
int getlocalnodeposition(m2m const &rel, int node, int localelement);

#endif
