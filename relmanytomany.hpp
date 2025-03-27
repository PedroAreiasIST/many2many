#ifndef RELBASIC_H
#define RELBASIC_H

#include <cstddef>
#include "relationonetomany.hpp"
#include "sek.hpp"

namespace
{
    using lst = sek<size_t>; // list of naturals (size_t values)
    using lst2 = sek<lst>; // list of lists of naturals
} // namespace

/**
 * @brief Represents a many-to-many relationship.
 *
 * The `relmanytomany` structure manages the complex relationships between
 * elements and nodes, wherein each element can correspond to multiple nodes
 * and each node can correspond to multiple elements.
 */
struct relmanytomany
{
    /**
     * @brief Manages element-to-node relationships in a one-to-many configuration.
     *
     * The `nodesfromelement` member is a structure representing the relationship
     * from elements to nodes in a one-to-many mapping. It is used to store and
     * efficiently access the nodes associated with each element. This facilitates
     * operations that involve traversing or manipulating the connections between
     * elements and nodes.
     */
    relationonetomany nodesfromelement;

    /**
     * @brief Defines a relationship from nodes to elements.
     *
     * The `elementsfromnode` member is a `relationonetomany` structure that holds
     * the node-to-element relationships. It maps nodes to their corresponding elements,
     * enabling efficient traversal and operations based on node associations within
     * a many-to-many relationship context.
     */
    relationonetomany elementsfromnode;

    // i.e. elementsfromnode[node][el] <= elementsfromnode[node][el+1]
    lst2 locn; /**<
                * @brief Stores node-element relationship indices for the many-to-many mapping.
                *
                * This structure tracks which elements are associated with each node.
                * In the context of a many-to-many relationship, it helps in establishing
                * the relationship between nodes and elements by recording each node's
                * local index for its connected elements.
                */

    size_t nnodes(size_t element);
    size_t nelems(size_t node);
};

PFR_FUNCTIONS_FOR(relmanytomany)

/**
 * @brief Defines a many-to-many relationship from a one-to-many mapping.
 *
 * Converts the one-to-many relationship stored in the `nodesfromelement` member
 * of a `relmanytomany` structure into a many-to-many relationship. It does this by
 * transposing the element-to-node relations into node-to-element relations, and
 * calculates the number of elements in each node along with their positions within nodes.
 *
 * @param rel The many-to-many relationship structure to be populated (modified in place).
 */
void setfromonetomany(relmanytomany &rel);

/**
 * @brief Retrieves a list of elements associated with a given list of nodes.
 *
 * Traverses the elements mapped to each node provided in the input list `nodes`
 * and returns the intersection of these element lists. The result is the set of
 * elements common to all nodes in the input list.
 *
 * @param rel   The many-to-many relationship structure containing node-to-element mappings.
 * @param nodes The list of nodes for which to retrieve associated elements.
 * @return A list of elements common to all provided nodes.
 */
lst getelementsfromnodes(relmanytomany const &rel, lst const &nodes);

/**
 * @brief Retrieves the neighboring elements of a given element in a many-to-many relationship.
 *
 * Finds all elements that share at least one node with the specified element (excluding the element itself).
 * The collected neighbors are stored in a list, which is then sorted and made unique.
 *
 * @param rel     The many-to-many relationship structure containing element-to-node (el→no)
 *                and node-to-element (no→el) mappings.
 * @param element The element whose neighbors are to be retrieved.
 * @return A list of neighboring elements that share nodes with the specified element.
 */
lst getneighbours(relmanytomany const &rel, size_t element);

/** @brief Generates a lexicographical order of elements. */
void lexiorder(relmanytomany const &rel, lst &orderofelements);

/**
 * @brief Generates an order of elements, with an option to transpose the relation.
 * @param transpose If true, compute order based on the transposed relation (nodes instead of elements).
 */
void toporder(relmanytomany const &rel, bool transpose, lst &order);

/**
 * @brief Computes index mapping arrays based on a given ordering of elements.
 *
 * Populates `oldfromnew` and `newfromold` such that they map indices from a new element order
 * back to the old order and vice versa.
 */
void indicesfromorder(relmanytomany const &rel, const lst &elementorder, lst &oldfromnew, lst &newfromold);

/**
 * @brief Compresses (removes) elements according to a mapping and updates the relationship.
 *
 * @param oldelementfromnew Mapping from new element indices to old element indices.
 */
void compresselements(relmanytomany &rel, lst const &oldelementfromnew);

/**
 * @brief Compresses (removes) nodes according to a mapping and updates the relationship.
 *
 * @param newnodefromold Mapping from new node indices to old node indices.
 */
void compressnodes(relmanytomany &rel, lst const &newnodefromold);

/**
 * @brief Provides read-only access to the `locn` structure.
 *
 * Returns a constant reference to the internal `locn` data of the given relationship,
 * allowing inspection of node-local indices without modification.
 *
 * @param rel The many-to-many relationship instance to query.
 * @return A constant reference to `rel.locn`.
 */
size_t getlocalnodeposition(relmanytomany const &rel, size_t node, size_t localelement);

#endif // RELBASIC_H
