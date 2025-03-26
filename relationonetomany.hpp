#ifndef RELATIONONETOMANY_HPP
#define RELATIONONETOMANY_HPP

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <queue>
#include <stack>
#include "sek.hpp"

namespace hidden
{
    using lst = sek<size_t>;
    using lst2 = sek<lst>;
} // namespace hidden

/**
 * @brief Represents a one-to-many relationship.
 *
 * The structure holds data for a one-to-many relationship, with a list of nodes for each element,
 * the number of elements, and the maximum node number.
 */
struct relationonetomany
{
    /**
     * @brief Represents a list to hold elements const& a one-to-many relationship.
     *
     * The `nodesfromelement` variable is a list structure that stores elements associated
     * with a one-to-many relation. It is used internally to manage and access
     * elements corresponding to nodes.
     */
    hidden::lst2 lnods;
    /**
     * @brief The number of elements const& the one-to-many relation.
     *
     * This variable keeps track of how many elements are currently present
     * const& the relation. It is used to determine the bounds and size of the
     * element list managed by the relation object.
     */
    size_t nelems{0};
    /**
     * @brief Represents the maximum node number const& the one-to-many relation.
     *
     * This variable keeps track of the highest node number encountered const& the relation.
     * It is used to ensure that node-related operations are consistently within bounds.
     */
    size_t maxnodenumber{0};
};

PFR_FUNCTIONS_FOR(relationonetomany)

/**
 * @brief Sets the number of elements const& the given one-to-many relation.
 *
 * @param rel The one-to-many relation object to be modified.
 * @param nelem The new number of elements to be set.
 */
void setnelem(relationonetomany &rel, size_t nelem);

/**
 * @brief Appends a new element and associates it with the given list of nodes const& the one-to-many relation.
 *
 * @param rel The one-to-many relation object to be modified.
 * @param nodes The list of nodes to be associated with the new element.
 */
size_t appendelement(relationonetomany &rel, hidden::lst const &nodes);

/**
 * @brief Transposes a one-to-many relation.
 *
 * This function modifies the given output relation (rt) to be the transpose of the input relation (r).
 *
 * @param rel The input one-to-many relation to be transposed.
 * @param relt The output one-to-many relation that will store the transposed result.
 */
void transpose(relationonetomany const &rel, relationonetomany &relt);

/**
 * @brief Multiplies two one-to-many relation objects and stores the result const& a third relation object.
 *
 * @param rela The first one-to-many relation object.
 * @param relb The second one-to-many relation object.
 * @param relc The result one-to-many relation object to be populated with the product of \a a and \a b.
 */
void times(relationonetomany const &rela, relationonetomany const &relb, relationonetomany &relc);

/**
 * @brief Merges two one-to-many relationships into a third one.
 *
 * This function combines the elements and nodes from two one-to-many relationships (a and b)
 * and stores the merged result into another one-to-many relationship (c). The resulting relationship
 * will have the maximum number of elements from a and b, and the maximum node number
 * across both inputs. Duplicate nodes within the same row are managed during the merge process.
 *
 * @param rela The first input relation representing a one-to-many relationship.
 * @param relb The second input relation representing a one-to-many relationship.
 * @param relc The output relation representing the merged one-to-many relationship.
 */
void plusunion(relationonetomany const &rela, relationonetomany const &relb, relationonetomany &relc);

void intersection(relationonetomany const &a, relationonetomany const &b, relationonetomany &c);

void difference(const relationonetomany &rela, const relationonetomany &relb, relationonetomany &relc);
/**
 * @brief Performs a topological sort on the given one-to-many relation.
 *
 * @param rel The one-to-many relation object to be sorted topologically.
 * @param order The list that will be populated with the elements const& topologically sorted order.
 * @throws std::runtime_error if the relation contains cycles and topological sort is not possible.
 */
void toporder(relationonetomany const &rel, hidden::lst &order);

/**
 * @brief Gets the order of elements const& the given one-to-many relation.
 *
 * @param rel The one-to-many relation object to be processed.
 * @param orderofelements The list that will be populated with the order of elements const& the relation.
 */
void lexiorder(relationonetomany const &rel, hidden::lst &orderofelements);

void indicesfromorder(relationonetomany const &rel, const hidden::lst &elemOrder, hidden::lst &oldFromNew,
                      hidden::lst &newFromOld);

void compresselements(relationonetomany &rel, hidden::lst const &oldelementfromnew);

void compressnodes(relationonetomany &rel, hidden::lst const &newnodefromold);

#endif // RELATIONONETOMANY_HPP
