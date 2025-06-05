#ifndef RELATIONMANAGER_HPP
#define RELATIONMANAGER_HPP
#include "mm2m.hpp"
#include "seque.hpp"
#include "symmetries.hpp"

/**
 * @struct thingmodel
 * @brief Represents a model containing nested structures for symmetry groups and children builders.
 *
 * The `thingmodel` structure is designed to encapsulate hierarchical data of a generic type.
 * It consists of two primary components: `symmetrygroups` and `childrenbuilders`,
 * both of which are represented as nested `seque` containers.
 *
 * @details
 * - `symmetrygroups`: Encodes hierarchical symmetry group information using nested sequences.
 * - `childrenbuilders`: Represents nested child builder hierarchies using deeper nested sequences.
 */
struct thingmodel
{
    seque<seque<seque<int> > >
    symmetrygroups; /**
     * @brief Represents a nested structure of symmetry groups.
     *
     * This variable is a three-level nested sequence (`seque<seque<seque<int>>>`)
     * that is used to organize and store information about symmetry groups.
     * Each level in the nested structure details hierarchy or associations
     * among symmetry group elements.
     */
    seque<seque<seque<seque<int> > > >
    childrenbuilders; /**
     *
     */
};

PFR_FUNCTIONS_FOR(thingmodel)

/**
 * @struct thing
 *
 * Represents a structure that includes a type number and an organizational collection.
 * This structure is used to store and manage hierarchical data based on types and associated nodes.
 */
struct thing
{
    /**
     * Represents the type number of a `thing` object.
     *
     * Initialized to -1 by default, indicating an unassigned state.
     * Must be set to a non-negative integer before appending elements
     * to the `typesandnodes` property of the `thing` object.
     *
     * Usage constraints:
     * - `typenumber` should start as -1 before being assigned a value.
     * - Once assigned, the value should remain non-negative.
     */
    int typenumber{-1};
    seque<seque<int> > typesandnodes; /**
     * A nested sequence structure that contains
     * sequences of integers.
     *
     * This variable represents a two-dimensional
     * container, where each inner sequence holds
     * integer values, and the outer sequence is used
     * to manage and organize multiple such inner sequences.
     */
};

PFR_FUNCTIONS_FOR(thing)

/**
 * Appends a symmetry group to the specified element and node type within the given sequence of thing models.
 *
 * @param em The sequence of thing models to modify.
 * @param elementtype The type of the element to which the symmetry group will be appended.
 * @param nodetype The type of the node within the element to which the symmetry group will be appended.
 * @param group The symmetry group to be appended, represented as a sequence of sequences of integers.
 */
void appendsymmetrygroup(seque<thingmodel> &em, int elementtype, int nodetype,
                         seque<seque<int> > const &group);

/**
 * Appends child nodes to the specified node in a sequence of models.
 *
 * @param em Reference to a sequence of thingmodel objects. It represents the collection of models being modified.
 * @param elementtype Integer representing the type of element in the sequence being targeted.
 * @param childtype Integer specifying the type of child node to append.
 * @param nodetype Integer representing the type of node within the child builders structure.
 * @param localnodesinem Constant reference to a sequence of integers representing the local nodes to append to the target node.
 */
void appendchildrenbuilder(seque<thingmodel> &em, int elementtype,
                           int childtype, int nodetype,
                           seque<int> const &localnodesinem);

/**
 * Sets the typenumber of a thing object to the specified element number
 * if its current typenumber is uninitialized (-1).
 *
 * @param e The thing object whose typenumber is to be set.
 * @param elementnumber The value to be assigned to the typenumber of the thing object.
 */
void settypenumberofathing(thing &e, int elementnumber);

/**
 * Appends elements to the specified type and node structure of the given object.
 *
 * @param e Reference to a `thing` object to which elements will be appended.
 * @param nodetype Integer specifying the type of node to be targeted for appending.
 * @param nodes A constant reference to a sequence of integers representing the nodes to append.
 */
void appendelement(thing &e, int nodetype, seque<int> const &nodes);

/**
 * Retrieves all child elements associated with a given 'thing' element within the specified model.
 *
 * This method processes the provided 'thing' element and the associated model data to generate
 * and return a sequence of all child elements. The child elements are determined based on the
 * children builders defined in the 'thingmodel' structure and their relationship to the provided
 * 'thing' element.
 *
 * @param element The target 'thing' object used as the parent element for generating child elements.
 * @param model The 'thingmodel' object defining the children builders and their relationships.
 * @return A sequence of 'thing' objects representing all child elements derived from the parent
 *         'thing' and based on the provided model.
 */
seque<thing> getallchildren(thing const &element,
                            thingmodel const &models);

/**
 * Uploads a thing into the provided mm2m structure, processes its types and nodes,
 * and appends the canonical forms to the structure based on the given model.
 *
 * @param m The mm2m data structure where the processed elements will be appended.
 * @param e The input `thing` containing the types and nodes to be processed.
 * @param model The `thingmodel` containing the symmetry groups required for canonical form calculation.
 */
void uploadathing(mm2m &m, thing const &e, thingmodel const &model);


#endif // RELATIONMANAGER_HPP
