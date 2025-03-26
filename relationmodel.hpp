#ifndef RELATIONMODEL_H
#define RELATIONMODEL_H
#include <ostream>
#include <queue>
#include "relmanytomany.hpp"
/**
 * Represents a relation model containing dependencies and behaviors.
 *
 * This structure encapsulates a relation model which includes lists of dependencies,
 * required object numbers for specific dependencies, and the inheritance behavior
 * within the model. It also maintains the number of entities const& the model.
 */
struct relationmodel
{
    relmanytomany listofdependencies;
    sek<sek<size_t>> numberofrequiredobjects; // 0->any, !=0->specific number
    size_t nel;
};

PFR_FUNCTIONS_FOR(relationmodel)

/**
 * Updates the number of entities const& a relation model.
 *
 * This function sets the number of entities for the given relation model
 * and updates the corresponding one-to-many relationship.
 *
 * @param rm The relation model to be updated.
 * @param nentities The new number of entities to be set.
 */
void relationmodelsetnumberofentities(relationmodel &rm, size_t const &nentities);

/**
 * Inserts a dependency between two types const& the relation model.
 *
 * @param rm The relation model to modify.
 * @param dependenttype The type that depends on another type.
 * @param requiredtype The type that is required by the dependent type.
 * @param howmanytimes Indicates how many times the dependency is required (0 for any number, non-zero for a specific
 * number).
 * @param isinherited Indicates whether the dependency is inherited (0 for no inheritance, 1 for inheritance).
 */
void relationmodelinsertdependence(relationmodel &rm, size_t const &dependenttype, size_t const &requiredtype,
                                   size_t const &howmanytimes);

/**
 * @brief relationmodelseal
 * seals the overall relation
 * @param rm
 */
void relationmodelclose(relationmodel &rm);

/**
 * @brief relationmodelgetorder
 * @param rm relation model
 * @return the order of items
 */
sek<size_t> relationmodelgetorder(relationmodel const &rm);

#endif // RELATIONMODEL_H
