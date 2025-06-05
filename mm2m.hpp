#ifndef RELATIONMATRIX_HPP
#define RELATIONMATRIX_HPP
#include "m2m.hpp"
#include "symmetries.hpp"
#include <map>
#include <stack>
#include <utility>

/**
 * @class mm2m
 * @brief Converts millimeters (mm) to meters (m).
 *
 * The `mm2m` class provides functionality to perform unit conversion
 * from millimeters to meters. This class simplifies calculations
 * where values in millimeters need to be represented in meters.
 *
 * The conversion is based on the standard metric conversion:
 * 1 millimeter = 0.001 meters.
 */
struct mm2m
{
    /**
     * @brief Represents a general-purpose variable named 'm'.
     *
     * This variable can be used to store or manipulate data relevant to the application.
     * The specific purpose, type, and usage of 'm' should be defined and documented
     * wherever this variable is implemented or assigned.
     */
    seque<seque<m2m> > m{};
    /**
     * @brief Represents the number of unique types in the mm2m structure.
     *
     * The `ntypes` variable defines the total count of distinct types available
     * for elements and nodes within the context of the `mm2m` structure. This
     * is utilized within the implementation to ensure proper indexing and
     * validation when accessing or manipulating data associated with element or
     * node types.
     *
     * Values for `ntypes` are expected to be non-negative and are used as bounds
     * for validating types in various operations.
     */
    int ntypes{0};
    /**
     * @brief A collection storing items marked as processed or flagged.
     *
     * This variable is used to maintain a list of elements that meet certain
     * criteria for being flagged or marked. The specific use case and nature
     * of the marks depend on the implementation or context in which this
     * variable is utilized.
     *
     * The structure and type of the collection may vary based on the requirements
     * of the system, but it is generally intended to offer efficient querying and
     * storage for elements that are considered "marked."
     */
    seque<std::pair<int, int> > listofmarked;

    /**
     * Overloads the operator for performing a specific operation between objects.
     *
     * @param other The object to be used in the operation with the current object.
     * @return A new object that is the result of the operation.
     */
    m2m const &operator()(int elementtype, int nodetype) const;

    /**
     * Overloads the operator for the specified functionality.
     *
     * @param other The object to be used in the operation with the current object.
     * @return The result of the operation between the current object and the provided object.
     */
    m2m &operator()(int elementtype, int nodetype);

    /**
     * Returns the number of nodes of a specific type associated with a given element type and element.
     *
     * @param elementType The type of the element for which the number of nodes needs to be determined.
     * @param element The specific element for which the number of nodes needs to be determined.
     * @param nodeType The type of the node to count for the specified element and element type.
     * @return The number of nodes of the given node type connected to the specified element and element type.
     */
    int nnodes(int elementtype, int element, int nodetype) const;

    /**
     * Determines the number of elements of a specified type connected to a given node
     * of a particular type.
     *
     * @param nodeType The type of the node for which connected elements are being queried.
     *                 Must be a non-negative integer and less than the total number of types (ntypes).
     * @param node The specific node of the given type. Must be non-negative and within
     *             the size of the node collection for the corresponding type.
     * @param elementType The type of elements to be considered. Must be a non-negative
     *                    integer and less than the total number of types (ntypes).
     * @return The number of elements of the specified type connected to the given node.
     *         Returns 0 if the node is out of range or not connected to any elements of
     *         the specified type.
     */
    int nelems(int nodetype, int node, int elementtype) const;

    /**
     * Counts the number of elements in the given container.
     *
     * This function calculates and returns the number of elements
     * present in the provided container. The container can be an
     * array or any data structure that supports iteration.
     *
     * @param container The container whose elements are to be counted.
     *                  It must support iteration.
     * @return The number of elements in the container.
     */
    int nelems(int elementtype) const;

    /**
     * Calculates the number of active elements in a given data structure.
     *
     * This method processes the provided data structure to count and return
     * the number of elements currently considered active based on its logic.
     *
     * @param data The data structure or container whose active elements need to be counted.
     *             The type of data should support iteration and means to determine active status.
     * @return The total count of active elements within the provided data structure.
     */
    int nactiveelements(int elementtype) const;
};

PFR_FUNCTIONS_FOR(mm2m)

/**
 * Marks a specific node of a given node type for removal within the mm2m data structure.
 * Updates the list of marked nodes in the mm2m object.
 *
 * @param m The mm2m object that contains the node relationships and list of marked nodes.
 * @param nodeType The type of the node being marked for removal.
 * @param node The specific node to be marked for removal.
 */
void marktoerase(mm2m &m, int nodetype, int node);

/**
 * @brief Identifies and marks duplicate node elements for erasure within a given mapping structure.
 *
 * This method examines the nodes associated with a specific element type and node type in the `mm2m` structure.
 * It determines the indices of duplicate nodes and marks them for removal, ensuring the structure is free
 * of redundant or repeated elements.
 *
 * @param m The `mm2m` data structure representing the mapping of elements and nodes.
 * @param elementtype The type of the element being analyzed.
 * @param nodetype The type of the node being analyzed.
 */
void marktoeraserepeated(mm2m &m, int elementtype, int nodetype);

/**
 * Retrieves all elements from the collection or data structure.
 *
 * @param none This method does not take any parameters.
 * @return A list or collection containing all the elements.
 */
seque<std::pair<int, int> > getallelements(mm2m const &m, int nodetype);

/**
 * Retrieves all elements from a collection or data structure.
 *
 * @param inputCollection The collection or data structure from which to retrieve all elements.
 *                        This parameter should be a valid container object.
 * @return A list or array containing all the elements retrieved from the provided collection.
 *         The order of elements will depend on the collection's traversal order.
 */
seque<std::pair<int, int> > getallelements(mm2m const &m, int nodetype,
                                           int node);

/**
 * Retrieves all unique and ordered nodes associated with a specific element type
 * and element number in the provided mesh-to-mesh mapping object.
 *
 * @param m The mesh-to-mesh mapping object containing the node and element relationships.
 * @param elementType The type of the element to retrieve nodes for.
 * @param elementNumber The specific element number for which nodes will be retrieved.
 * @return A sequence of unique and ordered node pairs. Each pair contains a node type
 *         and its corresponding identifier.
 */
seque<std::pair<int, int> > getallnodes(mm2m const &m, int elementtype,
                                        int element);

/**
 * Retrieves all unique nodes associated with a specific element type from the given mapping.
 *
 * This function iterates over all elements of the specified type in the mapping and
 * combines nodes associated with these elements into a single collection. The resulting
 * collection is ordered and contains only unique nodes.
 *
 * @param m A constant reference to an `mm2m` object which contains element-node mappings.
 * @param elementtype An integer representing the type of elements for which nodes are being retrieved.
 * @return A `seque` containing pairs of integers, where each pair represents a unique node identifier.
 */
seque<std::pair<int, int> > getallnodes(mm2m const &m, int elementtype);

namespace hidden
{
    /**
     *
     */
    seque<std::pair<int, int> >
    depthfirstsearchfromanode(mm2m const &m, std::pair<int, int> const &node);
}

/**
 * Sets the number of types to the specified value.
 *
 * @param numberOfTypes An integer representing the number of types to set.
 */
void setnumberoftypes(mm2m &m, int ntypes);

/**
 * Appends an element to the given container.
 *
 * @param container The container to which the element will be appended.
 *                   The container must support the push_back or equivalent method.
 * @param element The element to append to the container.
 * @return void This method does not return any value.
 */
int appendelement(mm2m &m, int elementtype, int nodetype,
                  seque<int> const &nodes);


/**
 * @brief Sets the number of elements for the specified element type in the mm2m structure.
 *
 * This function adjusts the number of elements for a given element type across all associated
 * node types within the mm2m structure. If the new number of elements is less than the current
 * number of elements for the specified element type, an exception is thrown.
 *
 * @param m The mm2m reference object to modify.
 * @param elementtype The type of element for which the number of elements is being set.
 * @param nelem The new number of elements to be assigned.
 */
void setnumberofelements(mm2m &m, int elementtype, int nelem);


/**
 * Retrieves a sequence of elements from the specified nodes by interacting with
 * the given matrix and element/node types.
 *
 * This function utilizes the provided matrix to select elements based on the
 * specified element type and node type, then filters the results using the
 * input nodes sequence.
 *
 * @param matrix Reference to the mm2m matrix object that holds element-to-node relationships.
 * @param elementType The type of element to be considered for retrieval.
 * @param nodeType The type of node to be considered for filtering elements.
 * @param nodes A constant reference to a sequence of node indices to filter relevant elements.
 * @return A sequence of integers representing the elements associated with the specified nodes.
 */
seque<int> getelementsfromnodes(mm2m &matrix, int elementtype, int nodestype,
                                seque<int> const &nodes);

/**
 * Retrieves elements with associated nodes from a given dataset or structure.
 *
 * This method is designed to identify and return elements that are linked to nodes,
 * filtering out elements without any associated nodes.
 *
 * @param dataset The input dataset or collection from which elements linked to nodes are to be retrieved.
 *                This parameter must contain a structure where elements and their nodes are defined.
 * @return A collection of elements that are associated with nodes,
 *         extracted from the provided dataset.
 */
seque<int> getelementswithnodes(mm2m &matrix, int elementtype, int nodestype,
                                seque<int> const &nodes);

/**
 * Compresses the given mm2m structure by reorganizing and eliminating redundant data.
 *
 * This method processes the internal data of the mm2m instance by modifying its
 * structures to ensure a consistent, optimized, and compact representation. It performs
 * operations such as depth-first search to mark nodes, organizing elements by type,
 * ensuring unique elements, and synchronizing data relationships across elements.
 *
 * @param m Reference to an mm2m instance that will be modified for compression.
 */
void compress(mm2m &m);

/**
 * @brief Retrieves the topological order of types based on their dependencies.
 *
 * This method computes the topological order of types present in the given mm2m
 * structure. It processes the type dependencies to determine a valid sequence of
 * types, ensuring that dependent types are ordered after the types they depend on.
 *
 * @param m The mm2m structure containing the types and their dependencies.
 *          The structure should include the number of types and the dependency
 *          relationships among them.
 * @return A sequence of integers representing the topological order of types.
 *         The sequence ensures that dependent types are ordered appropriately
 *         based on the provided dependency information.
 */
seque<int> gettypetoporder(mm2m const &m);

#endif // RELATIONMATRIX_HPP
