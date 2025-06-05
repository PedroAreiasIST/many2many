#ifndef RELATIONONETOMANY_HPP
#define RELATIONONETOMANY_HPP
#include "basics.hpp"
#include "seque.hpp"

/**
 * @class o2m
 * @brief Represents a one-to-many relationship model in a system.
 *
 * The o2m class is designed to facilitate the management and
 * representation of one-to-many relationships between objects.
 * It allows for mapping a single parent entity to multiple
 * associated child entities, providing functionality to add,
 * remove, or access these relationships.
 *
 * This class can be utilized predominantly in database models
 * or applications requiring structured relational management.
 */
struct o2m
{
    /**
     * @brief Represents a list of node identifiers or references.
     *
     * This variable is used to store a collection of nodes, which could be
     * utilized in various contexts such as graph structures, tree representations,
     * or mesh data in computational sciences.
     *
     * The specific type and structure of the nodes, as well as their application,
     * depend on the particular use case and implementation.
     */
    seque<seque<int> > lnods{{}};
    /**
     * @brief Represents the number of elements or items in a collection, array, or container.
     *
     * This variable is typically used to store the count of elements in a data structure
     * or to track the size of a collection. It is integral to loops, iterators, or any
     * functionality where the quantity of elements is required.
     *
     * Proper initialization and management of this variable are critical to ensure
     * its accuracy in representing the intended size.
     */
    int nelem{0};
    /**
     * @brief Represents the maximum number of nodes that can be processed or managed.
     *
     * This variable is used to define an upper limit on the total number of nodes
     * that can be handled within the system or algorithm. The value of maxnode
     * can be configured based on the specific requirements of the application,
     * ensuring optimal performance and preventing over-allocation of resources.
     *
     * It is commonly utilized in operations involving graph traversal, tree structures,
     * or other node-based data structures to impose a constraint on the problem domain.
     */
    int maxnode{0};
    /**
     * Overloads the operator for custom behavior.
     *
     * @param lhs The left-hand side operand of the operator.
     * @param rhs The right-hand side operand of the operator.
     * @return The result of the operation.
     */
    seque<int> &operator[](int element) { return lnods[element]; }
    /**
     * Overloads the operator for performing a specific operation.
     *
     * @param lhs The left-hand side operand involved in the operator operation.
     * @param rhs The right-hand side operand involved in the operator operation.
     * @return The result of the operation performed by the operator.
     */
    seque<int> const &operator[](int element) const { return lnods[element]; }
    /**
     * @brief Calculates the number of elements in an array or container.
     *
     * This method determines the total number of elements in a given array
     * or container. It is typically used to derive the count of elements
     * without manually iterating over them.
     *
     * @return The total number of elements as an integer.
     */
    int nelems() const { return nelem; }
    /**
     * Calculates the number of nodes in a given tree structure.
     *
     * @param root The root node of the tree from which to count the nodes.
     *             This should be a pointer to the root node or nullptr if the tree is empty.
     * @return The total number of nodes in the tree. Returns 0 if the tree is empty.
     */
    int nnodes(int element) const { return lnods[element].size; }
    //    REFLECT(o2m, lnods, nelem, maxnode);
};

PFR_FUNCTIONS_FOR(o2m)

/**
 * Sets the size of the specified object or collection.
 *
 * Adjusts the size to the given value, potentially altering the internal
 * structure or contents of the object or collection to match the new size.
 *
 * @param newSize The desired size to set the object or collection to.
 *                Must be a non-negative integer.
 * @throws invalid_argument If newSize is negative.
 * @throws runtime_error If resizing fails due to memory allocation issues
 *                       or other internal constraints.
 */
void setsize(o2m &rel, int nelem);

/**
 * Appends an element to the end of a collection or list.
 *
 * @param collection The collection or list to which the element is to be appended.
 * @param element The element to be added to the collection.
 * @return A boolean indicating whether the operation was successful.
 */
int appendelement(o2m &rel, const seque<int> &nodes);

/**
 * Overloads the operator to provide custom behavior when the operator is used.
 *
 * @param lhs The left-hand side operand of the operator.
 * @param rhs The right-hand side operand of the operator.
 * @return The result of the operation as per the custom implementation.
 */
o2m &operator<<(o2m &rel, std::initializer_list<int> nodes);

/**
 * Retrieves a list of duplicate elements found in the provided input list.
 * The method identifies duplicates based on their equality.
 *
 * @param inputList the list of elements to check for duplicates
 * @return a list containing the duplicate elements found in the input list
 */
seque<int> getduplicates(o2m const &rel);

/**
 * Calculates the sum of two integers.
 *
 * @param a The first integer to be added.
 * @param b The second integer to be added.
 * @return The sum of the two integers.
 */
o2m Tr(const o2m &rel);

/**
 * Overloads the operator to define a specific behavior for the operation.
 *
 * @param other An object or value to which this operator will be applied.
 *               The type and purpose of 'other' depend on the operator being overloaded.
 * @return The result of the operation, which can be a new object, a modified object,
 *         or a specific value depending on the operator logic.
 */
o2m operator*(const o2m &rela, const o2m &relb);

/**
 * Overloaded operator for performing a specific operation.
 *
 * @param lhs The left-hand side operand of the operation.
 * @param rhs The right-hand side operand of the operation.
 * @return The result of the operation between the left-hand and right-hand operands.
 */
o2m operator*(const o2m &rela, const seque<int> &vec);

/**
 * Overloaded operator for a specific functionality.
 *
 * This operator is used to define custom behavior when the operator is invoked
 * on instances of this class or type. The exact implementation depends on the
 * type of operation the operator is intended to perform.
 *
 * @param other The operand or argument to be used in conjunction with the operator.
 *              This may be another instance of the class/type or a different type entirely,
 *              depending on the operator being overloaded.
 * @return The result of applying the operator. The return type and value
 *         depend on the specific implementation of the operator.
 */
o2m operator+(const o2m &rela, const o2m &rel);

/**
 * Overloads the operator to enable custom behavior for a specific operation.
 *
 * @param lhs The left-hand side operand involved in the operator invocation.
 * @param rhs The right-hand side operand involved in the operator invocation.
 * @return The result of the operation performed by the overloaded operator.
 */
o2m operator||(const o2m &a, const o2m &b);

/**
 * Overloads the operator to define a custom behavior for a specific operation.
 *
 * @param lhs The left-hand side operand involved in the operation.
 * @param rhs The right-hand side operand involved in the operation.
 * @return The result of the custom operation as defined by the operator overload.
 */
o2m operator&&(const o2m &a, const o2m &b);

/**
 * Overloads the operator for a specific functionality.
 *
 * @param lhs The left-hand side operand of the operator.
 * @param rhs The right-hand side operand of the operator.
 * @return The result of applying the operator on the operands.
 */
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

/**
 * Retrieves the details of an order based on the provided order identifier.
 *
 * @param orderId A unique identifier for the order to be retrieved.
 * @return The details of the order as an order object or null if the order is not found.
 */
seque<int> getorder(const o2m &rel);

namespace hidden
{
    /**
     * Compresses the given array of elements by replacing consecutive duplicate elements
     * with a single instance of that element.
     *
     * @param elements The array of elements to be compressed. Must not be null.
     * @param size The size of the input array. Must be a non-negative integer.
     * @return The new size of the compressed array.
     */
    void compresselements(o2m &rel, const seque<int> &oldelementfromnew);

    /**
     * Permutes the nodes of a graph or tree*/
    void permutenodes(o2m &rel, const seque<int> &newnodefromold);

    /**
     * Retrieves the positions of nodes from a data structure or a graph.
     *
     * @param nodes A collection of nodes for which the positions are to be determined.
     *              This could be a list, set, or any other data structure containing nodes.
     * @return A collection of positions corresponding to the input nodes.
     *         Each position represents the location data or metadata associated with a node.
     */
    seque<seque<int> > getnodepositions(o2m const &nodesfromelement,
                                        o2m const &elementsfromnode);

    /**
     * Retrieves the positions of all occurrences of a specified element within a collection.
     *
     * @param collection The collection in which to search for the specified element.
     * @param element The element whose positions are to be found within the collection.
     * @return A vector containing the positions of the specified element within the collection.
     *         If the element is not found, the vector will be empty.
     */
    seque<seque<int> > getelementpositions(o2m const &nodesfromelement,
                                           o2m const &elementsfromnode);
} // namespace hidden

/**
 * Retrieves a one-to-many mapping from a provided sequence.
 *
 * @param sequence A collection or iterable input from which the one-to-many mapping is derived.
 * @return A one-to-many mapping object constructed based on the given sequence.
 */
o2m geto2mfromsequence(const seque<int> &other);

/**
 * Retrieves all cliques (maximally connected subgraphs) from the given graph.
 *
 * @param graph The input graph represented as an adjacency matrix or adjacency list.
 * @return A collection of cliques, where each clique is represented as a list or set of nodes.
 */
seque<seque<int> > getcliques(const o2m &nodesfromelement,
                              const o2m &elementsfromnode);

#endif
