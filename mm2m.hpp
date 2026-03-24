#ifndef RELATIONMATRIX_HPP
#define RELATIONMATRIX_HPP
#include "m2m.hpp"
#include "symmetries.hpp"
#include <map>
#include <stack>
#include <utility>

/**
 * @class mm2m
 * @brief Represents a matrix of many-to-many relationships across multiple types.
 *
 * Manages heterogeneous element-node relationships via a 2D matrix of m2m objects,
 * indexed by (elementtype, nodetype). Supports element lifecycle management
 * (marking, compression) and topological ordering of types.
 */
struct mm2m
{
    /** 2D matrix of m2m relations, indexed as m[elementtype][nodetype]. */
    seque<seque<m2m> > m{};
    /** Total number of distinct entity types (dimension of the m matrix). */
    int ntypes{0};
    /** (type, id) pairs marked for erasure during the next compress(). */
    seque<std::pair<int, int> > listofmarked;

    m2m const &operator()(int elementtype, int nodetype) const;
    m2m &operator()(int elementtype, int nodetype);

    int nnodes(int elementtype, int element, int nodetype) const;
    int nelems(int nodetype, int node, int elementtype) const;
    int nelems(int elementtype) const;
    /** Counts elements of the given type that have non-empty node lists. */
    int nactiveelements(int elementtype) const;
};

PFR_FUNCTIONS_FOR(mm2m)

void marktoerase(mm2m &m, int nodetype, int node);

/** Finds duplicate elements (by node connectivity) and marks them for erasure. */
void marktoeraserepeated(mm2m &m, int elementtype, int nodetype);

/** Returns all (type, id) element pairs connected to nodes of the given type. */
seque<std::pair<int, int> > getallelements(mm2m const &m, int nodetype);
seque<std::pair<int, int> > getallelements(mm2m const &m, int nodetype, int node);

/** Returns all (type, id) node pairs connected to a given element. */
seque<std::pair<int, int> > getallnodes(mm2m const &m, int elementtype, int element);
seque<std::pair<int, int> > getallnodes(mm2m const &m, int elementtype);

namespace hidden
{
    seque<std::pair<int, int> >
    depthfirstsearchfromanode(mm2m const &m, std::pair<int, int> const &node);
}

void setnumberoftypes(mm2m &m, int ntypes);

/** @return Index of the newly appended element. */
int appendelement(mm2m &m, int elementtype, int nodetype,
                  seque<int> const &nodes);

void setnumberofelements(mm2m &m, int elementtype, int nelem);

seque<int> getelementsfromnodes(mm2m &matrix, int elementtype, int nodestype,
                                seque<int> const &nodes);
seque<int> getelementswithnodes(mm2m &matrix, int elementtype, int nodestype,
                                seque<int> const &nodes);

/** Erases marked elements and their dependents (via DFS), then re-synchronizes. */
void compress(mm2m &m);

/** Topological ordering of types based on their dependency relationships. */
seque<int> gettypetoporder(mm2m const &m);

#endif // RELATIONMATRIX_HPP
