#ifndef RELMANYTOMANY_HPP
#define RELMANYTOMANY_HPP

#include "o2m.hpp"
#include "seque.hpp"
#include "superstruct.hpp"
#include <cstddef>

/**
 * @brief Bidirectional many-to-many relation between elements and nodes.
 *
 * Holds the forward table (nfrome: element -> nodes), its symbolic transpose
 * (efromn: node -> elements, rows sorted) and, on demand, the two tables of
 * local positions of Section 2.2 (nodeloc, elementloc). The transpose is
 * rebuilt by synchronize() after a batch of mutations; the positions are
 * computed in one parallel pass the first time they are requested.
 */
struct m2m
{
    /** Element-to-node table. */
    o2m nfrome;
    /** Node-to-element table, Tr(nfrome). */
    o2m efromn;
    /** nodeloc[node][k]: local position of node in element efromn[node][k]. */
    o2m nodeloc;
    /** elementloc[element][i]: position of element in the list of node nfrome[element][i]. */
    o2m elementloc;
    /** False when nfrome has changed since the last synchronize(). */
    bool isupdated{false};
    /** False when nodeloc/elementloc are stale. */
    bool islocated{false};
};

PFR_FUNCTIONS_FOR(m2m)

/** @return Index of the newly appended element. */
int appendelement(m2m &rel, crowview nodes);

/** Sets the number of elements (new rows empty). */
void setnumberofelements(m2m &rel, int nelem);

/** Rebuilds efromn from nfrome if needed. */
void synchronize(m2m &rel);

/** Rebuilds the local-position tables if needed (after synchronize). */
void synchronizelocations(m2m &rel);

/** Elements that contain ALL the given nodes (smallest list first, sorted merge). */
seque<int> getelementswithnodes(m2m &rel, crowview nodes);

/** Elements whose node set is exactly the given list. */
seque<int> getelementsfromnodes(m2m &rel, crowview nodes);

seque<int> getelementneighbors(m2m &rel, int element);
seque<int> getnodeneighbors(m2m &rel, int node);

/** Lexicographic ordering permutation of the elements. */
seque<int> getorder(m2m const &rel);

/** Topological ordering of the relation. Throws if cycles exist. */
seque<int> gettoporder(m2m const &rel);

void compresselements(m2m &rel, seque<int> const &oldelementfromnew);
void permutenodes(m2m &rel, seque<int> const &newnodefromold);

/** nfrome * efromn: element-to-element adjacency through shared nodes. */
m2m getelementstoelements(m2m &rel);

/** efromn * nfrome: node-to-node adjacency through shared elements. */
m2m getnodestonodes(m2m &rel);

/** Clique addressing of the element-node connectivity. */
o2m getcliques(m2m &rel);

#endif
