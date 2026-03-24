#ifndef RELATIONONETOMANY_HPP
#define RELATIONONETOMANY_HPP
#include "basics.hpp"
#include "seque.hpp"

/**
 * @brief One-to-many relation: maps elements to their node lists.
 *
 * Each element (row) has a variable-length list of node indices.
 * Supports sparse algebraic operations (*, +, -, &&), transpose, and topological ordering.
 */
struct o2m
{
    /** Per-element node lists: lnods[element] = {node0, node1, ...}. */
    seque<seque<int> > lnods{{}};
    int nelem{0};
    /** Highest node index seen across all elements. */
    int maxnode{0};
    seque<int> &operator[](int element) { return lnods[element]; }
    seque<int> const &operator[](int element) const { return lnods[element]; }
    int nelems() const { return nelem; }
    int nnodes(int element) const { return lnods[element].size; }
};

PFR_FUNCTIONS_FOR(o2m)

void setsize(o2m &rel, int nelem);

/** @return Index of the newly appended element. */
int appendelement(o2m &rel, const seque<int> &nodes);

/** Appends an element from an initializer list of node ids. */
o2m &operator<<(o2m &rel, std::initializer_list<int> nodes);

/** Returns indices of elements whose node lists appear more than once. */
seque<int> getduplicates(o2m const &rel);

/** Transpose: returns node-to-element mapping from an element-to-node relation. */
o2m Tr(const o2m &rel);

/** Sparse symbolic multiplication of two o2m relations. */
o2m operator*(const o2m &rela, const o2m &relb);

/** Multiplies a relation by a vector (converts vec to identity o2m first). */
o2m operator*(const o2m &rela, const seque<int> &vec);

/** Row-wise union of two relations. */
o2m operator+(const o2m &rela, const o2m &rel);

/** Alias for operator+ (row-wise union). */
o2m operator||(const o2m &a, const o2m &b);

/** Row-wise intersection of two relations. */
o2m operator&&(const o2m &a, const o2m &b);

/** Row-wise set difference (rela \ relb). */
o2m operator-(const o2m &rela, const o2m &relb);

/**
 * Retrieves the topological order of the elements in a directed acyclic graph.
 *
 * This method computes a topological ordering of the vertices in the
 * input directed acyclic graph (DAG). Each vertex appears before all the
 * vertices it points to directly or indirectly in the graph. The method
 * assumes that the input graph is a valid DAG and does not handle cycles.
 *
 * @param graph The representation of the graph, typically as an adjacency list,
 *              where the keys are node identifiers and the values are lists of
 *              nodes that the key node has outgoing edges to.
 *              For example, a graph {A -> [B, C], B -> [D], C -> [], D -> []}
 *              means A points to B and C, B points to D, C and D have no outgoing edges.
 * @return A vector or list that represents the topological order of the nodes
 *         in the graph. If the graph has N nodes, the result will contain all
 *         N nodes, sorted such that dependencies are respected.
 */
seque<int> gettoporder(const o2m &rel);

/** Returns a lexicographic ordering permutation of the relation's rows. */
seque<int> getorder(const o2m &rel);

namespace hidden
{
    /** Reorders elements according to oldelementfromnew mapping and updates nelem/maxnode. */
    void compresselements(o2m &rel, const seque<int> &oldelementfromnew);

    /** Replaces node indices using newnodefromold permutation. */
    void permutenodes(o2m &rel, const seque<int> &newnodefromold);

    /** For each node, returns the local positions within each element it belongs to. */
    seque<seque<int> > getnodepositions(o2m const &nodesfromelement,
                                        o2m const &elementsfromnode);

    /** For each element, returns which position in each node's adjacency list it occupies. */
    seque<seque<int> > getelementpositions(o2m const &nodesfromelement,
                                           o2m const &elementsfromnode);
} // namespace hidden

/** Creates an identity o2m from a sequence (each element maps to itself). */
o2m geto2mfromsequence(const seque<int> &other);

/** Computes clique numbering for element-node connectivity. */
seque<seque<int> > getcliques(const o2m &nodesfromelement,
                              const o2m &elementsfromnode);

#endif
