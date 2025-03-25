#ifndef RELBASIC_H
#define RELBASIC_H
#include <cstddef>
#include "argsstart.hpp"
#include "relationonetomany.hpp"
#include "sek.hpp"
namespace
{
    using lst = sek<size_t>; // list of naturals
    using lst2 = sek<lst>; // list of list of naturals
} // namespace

/**
 * @brief Represents a many-to-many relationship.
 *
 * The `relmanytomany` structure manages the complex relationships between elements and nodes
 * wherein each element can correspond to multiple nodes and each node can correspond to multiple elements.
 */
struct relmanytomany
{
    /**
     * @brief Manages element-to-node relationships in a one-to-many configuration.
     *
     * The `nodesfromelement` variable is a structure representing the relationship from elements
     * to nodes in a one-to-many mapping. It is used to store and efficiently access
     * the nodes associated with each element. This facilitates operations that involve
     * traversing or manipulating the connections between elements and nodes.
     */
    relationonetomany nodesfromelement;
    /**
     * @brief Defines a relationship from nodes to elements.
     *
     * The variable `elementsfromnode` represents a `relationonetomany` structure that holds the node-to-element
     * relationships. It is used to map nodes to the corresponding elements, enabling efficient
     * traversal and operations based on node associations within a many-to-many relationship context.
     */
    relationonetomany elementsfromnode;
    // i.e. elementsfromnode[node][el]<=elementsfromnode[node][el+1]
    lst2 locn; /**
                * @brief Stores node-element relationships for a many-to-many relationship.
                *
                * This variable is used to track which elements are associated with each node.
                * In the context of a many-to-many relationship, this structure helps in establishing
                * the relationship between nodes and elements by recording the node's local element indices.
                */
};

PFR_FUNCTIONS_FOR(relmanytomany)

/**
 * @brief Defines a many-to-many relationship from a one-to-many relationship.
 *
 * This function converts the one-to-many relationship stored in the `nodesfromelement`
 * member of `relmanytomany` structure to a many-to-many relationship. It achieves
 * this by transposing the element-to-node relations into node-to-element relations,
 * and calculates the number of elements in each node, along with their positions
 * within nodes.
 *
 * @param rel The many-to-many relationship structure to be defined. This structure
 *        is modified in place.
 */
void setfromonetomany(relmanytomany &rel);

/**
 * @brief Retrieves a list of elements associated with a given list of nodes.
 *
 * This function traverses the elements mapped to each node provided in the input
 * list 'nodes' and returns the intersection of these element lists. The intersection
 * represents the common elements shared by all nodes in the input list.
 *
 * @param rel The many-to-many relationship structure containing node-to-element mappings.
 * @param nodes The list of nodes for which to retrieve the associated elements.
 * @return A list of elements common to all provided nodes.
 */
lst getelementsfromnodes(relmanytomany const &rel, lst const &nodes);

/**
 * @brief Retrieves the neighboring elements of a given element in a many-to-many relationship.
 *
 * This function finds all elements that share nodes with a specified element, except
 * for the element itself. The neighbors are stored in a list which is then sorted
 * and made unique.
 *
 * @param rel The many-to-many relationship structure containing element-to-node (elno)
 *        and node-to-element (noel) relations.
 * @param element The element whose neighbors are to be retrieved.
 * @return A list of neighboring elements that share nodes with the specified element.
 */
lst getneighbours(relmanytomany const &rel, size_t element);

/**
 * @brief Computes node-to-node relationships for a given many-to-many relationship.
 *
 * This function calculates the intermediate node-to-node relationships from the
 * element-to-node and node-to-element mappings provided by the `relmanytomany` structure.
 *
 * @param rel The many-to-many relationship structure containing nodes-from-element and elements-from-node mappings.
 * @param nodetonode An output parameter that stores the computed node-to-node relationships.
 */
void getnodestonodes(relmanytomany const &rel, relationonetomany &nodetonode);

/**
 * @brief Retrieves the element-to-element relationship from a many-to-many relationship.
 *
 * This function calculates the relationship between elements based on a given
 * many-to-many `relmanytomany` structure and populates the `relationonetomany` structure with the results.
 *
 * @param rel The many-to-many relationship structure containing element-to-node and node-to-element relationships.
 * @param elementtoelement The one-to-many relationship structure where the resulting element-to-element
 *        relationship will be stored. This structure will be modified by the function.
 */
void getelementstoelements(relmanytomany const &rel, relationonetomany &elementtoelement);

void lexiorder(relmanytomany const &rel, lst &orderofelements);

void toporder(relmanytomany const &rel, bool transpose, lst &order);

void indicesfromorder(relmanytomany const &rel, const lst &elemOrder, lst &oldFromNew, lst &newFromOld);

void compresselements(relmanytomany &rel, lst const &oldelementfromnew);

void compressnodes(relmanytomany &rel, lst const &newnodefromold);

#endif // RELBASIC_H
