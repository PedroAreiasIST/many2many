#ifndef RELATIONMATRIX_HPP
#define RELATIONMATRIX_HPP

#include <map>
#include <stack>
#include <utility>
#include "relmanytomany.hpp"
#include "symmetries.hpp"

/**
 * @brief Structure representing a relation matrix.
 */
namespace hidden
{
    using matrix = sek<relmanytomany>;
}

struct relationmatrix
{
    hidden::matrix m{};
    sek<sek<sek<sek<size_t>>>> groups{{}};
    size_t ntypes{0};
    // Access the many-to-many relationship for a given element and node type.
    relmanytomany &operator()(size_t elementtype, size_t nodetype)
    {
        size_t minimum = std::min(nodetype, elementtype);
        size_t maximum = std::max(nodetype, elementtype);
        size_t index = minimum * ntypes - (minimum - 1) * minimum / 2 + maximum - minimum;
        return m[index];
    }

    size_t nnode(size_t elementtype, size_t element, size_t nodetype);
    size_t nelem(size_t nodetype, size_t node, size_t elementtype);
};

PFR_FUNCTIONS_FOR(relationmatrix)

/**
 * @brief Retrieves all elements associated with the given node.
 *
 * @param m The relation matrix.
 * @param node A pair where first is nodetype and second is node number.
 * @return A list of (element type, element number) pairs.
 */
sek<std::pair<size_t, size_t>> getallelements(relationmatrix &m, std::pair<size_t, size_t> const &node);

/**
 * @brief Retrieves all nodes associated with the given element.
 *
 * @param m The relation matrix.
 * @param element A pair where first is element type and second is element number.
 * @return A list of (node type, node number) pairs.
 */
sek<std::pair<size_t, size_t>> getallnodes(relationmatrix &m, std::pair<size_t, size_t> const &element);

/**
 * @brief Depth-first search starting from a given node.
 *
 * @param m The relation matrix.
 * @param node A pair where first is nodetype and second is node number.
 * @return A list of (node type, node number) pairs reachable from the start node.
 */
sek<std::pair<size_t, size_t>> depthfirstsearchfromanode(relationmatrix &m, std::pair<size_t, size_t> const &node);

/**
 * @brief Depth-first search across all node types starting from a given node.
 *
 * @param m The relation matrix.
 * @param node The starting node number.
 * @return A list of (node type, node number) pairs reachable from the start node.
 */
sek<std::pair<size_t, size_t>> depthfirstsearch(relationmatrix &m, size_t node);

/**
 * @brief Sets the number of types in the relation matrix.
 *
 * @param m The relation matrix.
 * @param ntypes The number of types.
 */
void setnumberoftypes(relationmatrix &m, size_t ntypes);

/**
 * @brief Sets the symmetry group for a specific element-node type pair.
 *
 * @param m The relation matrix.
 * @param elementype The element type.
 * @param nodetype The node type.
 * @param group The symmetry group.
 */
void setsymmetrygroup(relationmatrix &m, size_t elementype, size_t nodetype, sek<sek<size_t>> const &group);

/**
 * @brief Appends an element to the relation matrix.
 *
 * @param m The relation matrix.
 * @param elementype The element type.
 * @param nodetype The node type.
 * @param nodes The list of nodes associated with the element.
 * @return The index of the appended element.
 */
size_t appendelement(relationmatrix &m, size_t elementype, size_t nodetype, sek<size_t> const &nodes);

/**
 * @brief Computes a lexicographical ordering for a given element-node type pair.
 *
 * @param m The relation matrix.
 * @param elementype The element type.
 * @param nodetype The node type.
 * @param order Output parameter receiving the ordering.
 */
void lexiorder(relationmatrix &m, size_t elementype, size_t nodetype, sek<size_t> &order);

/**
 * @brief Computes index mappings from a given order.
 *
 * @param m The relation matrix.
 * @param elementtype The element type.
 * @param nodetype The node type.
 * @param order The new ordering.
 * @param oldfromnew Mapping from new indices to old.
 * @param newfromold Mapping from old indices to new.
 */
void indicesfromorder(relationmatrix &m, size_t elementtype, size_t nodetype, sek<size_t> const &order,
                      sek<size_t> &oldfromnew, sek<size_t> &newfromold);

/**
 * @brief Closes the element-node relationship.
 *
 * @param m The relation matrix.
 * @param elementype The element type.
 * @param nodetype The node type.
 */
void closeelementnoderelation(relationmatrix &m, size_t elementype, size_t nodetype);

void closeeverything(relationmatrix &m);

/**
 * @brief Retrieves elements from nodes using symmetry reduction.
 *
 * @param matrix The relation matrix.
 * @param elementtype The element type.
 * @param nodestype The node type.
 * @param nodes The list of nodes.
 * @return A list of elements.
 */
sek<size_t> getselementsfromnodes(relationmatrix &matrix, size_t elementtype, size_t nodestype,
                                  sek<size_t> const &nodes);

/**
 * @brief Compresses elements in the relation matrix.
 *
 * @param m The relation matrix.
 * @param elementtype The element type.
 * @param oldelementfromnew Mapping from old element indices to new.
 * @param newelementfromold Mapping from new element indices to old.
 */
void compress(relationmatrix &m, size_t elementtype, sek<size_t> const &oldelementfromnew,
              sek<size_t> const &newelementfromold);

#endif // RELATIONMATRIX_HPP
