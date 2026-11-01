#ifndef M2M_GRAPH_HPP
#define M2M_GRAPH_HPP
// Derived queries on relations: dual graphs by shared count, connected components,
// breadth-first levels, k-hop neighborhoods, reverse Cuthill-McKee ordering,
// bandwidth and greedy coloring. Translated from the typed C# library of the
// authors (Topology.cs) onto the CSR kernels of o2m.
#include "m2m.hpp"
#include "o2m.hpp"
#include "seque.hpp"

/** Element-to-element adjacency through at least \p minshared common nodes (the
 *  composition nfrome * efromn with multiplicities, filtered). Row e lists the
 *  neighbors of e in order of first appearance, e itself excluded. If \p counts
 *  is given it receives, in the same layout, the number of shared nodes. */
o2m getdualgraph(m2m &rel, int minshared, o2m *counts = nullptr);

/** Node-to-node adjacency through shared elements (efromn * nfrome), diagonal removed. */
o2m getnodegraph(m2m &rel);

/** Neighbors of one element with at least \p minshared common nodes (sorted);
 *  \p counts, if given, receives the shared counts in the same order. */
seque<int> getelementssharing(m2m &rel, int element, int minshared, seque<int> *counts = nullptr);

/** Elements within \p k hops of \p element in the dual graph of threshold \p minshared
 *  (the element itself at hop 0 included), sorted. */
seque<int> getkhopneighborhood(m2m &rel, int element, int k, int minshared = 1);

/** Connected components of a symmetric adjacency (CSR rows = vertices). label[v] in
 *  [0, ncomp), numbered in order of the smallest vertex of each component. */
int getcomponents(o2m const &graph, seque<int> &label);

/** Connected components of the elements through shared nodes, without forming the dual
 *  graph (union-find on the incidences). elementlabel and nodelabel receive the ids;
 *  isolated nodes get their own components after the elements'. */
int getcomponents(m2m &rel, seque<int> &elementlabel, seque<int> &nodelabel);

/** Breadth-first levels from \p source in a symmetric adjacency (-1 unreachable). */
seque<int> getbfslevels(o2m const &graph, int source);

/** Reverse Cuthill-McKee ordering of a symmetric adjacency: the vertex at new position i
 *  is oldfromnew[i]; starts from a pseudo-peripheral vertex of every component. */
seque<int> getrcmorder(o2m const &graph);

/** Bandwidth of a symmetric adjacency under the permutation oldfromnew (identity if empty). */
int getbandwidth(o2m const &graph, seque<int> const &oldfromnew = seque<int>());

/** Greedy first-fit coloring of a symmetric adjacency in vertex order; returns the
 *  number of colors, color[v] in [0, ncolors). */
int getcoloring(o2m const &graph, seque<int> &color);

/** Appends an element and keeps the transpose valid (the transpose becomes jagged;
 *  the local positions are invalidated). @return the element index. */
int appendelementsynchronized(m2m &rel, crowview nodes);

#endif
