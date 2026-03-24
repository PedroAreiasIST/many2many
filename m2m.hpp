#ifndef RELMANYTOMANY_HPP
#define RELMANYTOMANY_HPP

#include "o2m.hpp"
#include "seque.hpp"
#include "superstruct.hpp"
#include <cstddef>

/**
 * @brief Bidirectional many-to-many relation between elements and nodes.
 *
 * Maintains both the forward mapping (nfrome: elements→nodes) and the
 * reverse mapping (efromn: nodes→elements), with lazy synchronization.
 */
struct m2m
{
    /** Element-to-node mapping (nodes from elements). */
    o2m nfrome;
    /** Node-to-element mapping (elements from nodes), derived via Tr(nfrome). */
    o2m efromn;
    /** For each node, its local position within each element it belongs to. */
    seque<seque<int> > nodeloc;
    /** For each element, the position in each node's adjacency list. */
    seque<seque<int> > elementloc;
    /** False when nfrome has changed and efromn/nodeloc/elementloc need recomputation. */
    bool isupdated{false};
};

PFR_FUNCTIONS_FOR(m2m)

/** @return Index of the newly appended element. */
int appendelement(m2m &rel, seque<int> const &nodes);

void setnumberofelements(m2m &rel, int nelem);

/** Recomputes efromn, nodeloc, and elementloc from nfrome if needed. */
void synchronize(m2m &rel);

/** Returns elements that contain ALL the given nodes. */
seque<int> getelementswithnodes(m2m const &rel, seque<int> const &nodes);

/** Returns elements whose node set exactly matches the given nodes. */
seque<int> getelementsfromnodes(m2m const &rel, seque<int> const &nodes);

seque<int> getelementneighbours(m2m const &rel, int element);
seque<int> getnodeneighbours(m2m const &rel, int node);

/** Returns a lexicographic ordering permutation. */
seque<int> getorder(m2m const &rel);

/** Topological ordering of the relation. Throws if cycles exist. */
seque<int> gettoporder(m2m const &rel);

void compresselements(m2m &rel, seque<int> const &oldelementfromnew);
void permutenodes(m2m &rel, seque<int> const &newnodefromold);

/** Returns nfrome * efromn: element-to-element adjacency through shared nodes. */
m2m getelementstoelements(m2m &rel);

/** Returns efromn * nfrome: node-to-node adjacency through shared elements. */
m2m getnodestonodes(m2m &rel);

/** Computes clique numbering for the element-node connectivity. */
seque<seque<int> > getcliques(m2m &rel);

#endif
