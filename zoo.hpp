#pragma once
#include "mm2m.hpp"
#include "superstruct.hpp"
#include "thing.hpp"
#include "typseque.hpp"

template<typename TypeList>
/**
 * @brief The zoo class is responsible for managing a system of elements,
 * nodes, and their interactions such as symmetry groups, children builders,
 * and relationships. It utilizes a TypeManager to organize type-based
 * structures and operations.
 */
struct zoo
{
    using TypeManager = typsequetostructtype<TypeList, superstruct>;
    /**
     * @brief Represents a life management system within the zoo structure.
     *
     * This variable is designed to handle operations and data related to managing
     * the life cycles or related aspects within the context of the zoo system.
     */
    mm2m lifemanager;
    /**
     * @brief Represents a variable named `synthesized` of type `mm2m`.
     *
     * The purpose or intended usage of this variable is not specified in
     * the given code snippet. It is part of the `zoo` structure and may
     * serve as a key element or configuration point based on the broader
     * context of the application.
     */
    mm2m synthesized;
    /**
     * @brief Represents a type manager for handling mesh-related data and operations.
     *
     * The `mesh` variable is used within the `zoo` structure to manage and interact
     * with a collection of data or structures organized as a mesh. This can include
     * elements, nodes, or any related entities present in the computational or logical
     * representation of a mesh. Operations such as node insertion, symmetry group
     * definitions, and uploading elements likely utilize this variable.
     *
     * This variable is defined as `TypeManager`, which is derived from a specialized
     * type transformation (`typsequetostructtype`) based on the `TypeList` and
     * `superstruct` constructs.
     *
     * @note Ensure consistency and correctness when manipulating or accessing
     *       `mesh`, as it forms the core data structure in related functions.
     */
    TypeManager mesh;
    /**
     * A sequence container storing objects of type `thingmodel`.
     *
     * This variable provides an ordered collection of `thingmodel` objects
     * that can be accessed, modified, and iterated over using the methods
     * provided by the `seque` container. It is utilized for managing and
     * organizing instances of `thingmodel` within the zoo system.
     */
    seque<thingmodel> thingmodels;

    /**
     * Constructor for the zoo class. Initializes internal data structures and configurations
     * for managing various types, including symmetry groups and children builders for each type.
     * The sizes of different components are set based on the constants defined in the TypeManager.
     *
     * @return An instance of the zoo class with properly initialized data structures.
     */
    explicit zoo()
    {
        setnumberoftypes(lifemanager, TypeManager::Size);
        setnumberoftypes(synthesized, TypeManager::Size);
        setsize(thingmodels, TypeManager::Size);
        for (int type = 0; type < TypeManager::Size; ++type)
        {
            setsize(thingmodels[type].symmetrygroups, TypeManager::Size);
            setsize(thingmodels[type].childrenbuilders, TypeManager::Size);
            for (int othertype = 0; othertype < TypeManager::Size; ++othertype)
            {
                setsize(thingmodels[type].childrenbuilders[othertype],
                        TypeManager::Size);
            }
        }
    }

    // model definition
    template<typename E, typename C, typename N>
    /**
     * Defines a children builder for a specific model.
     *
     * @tparam E The type representing elements in the model.
     * @tparam C The type representing components in the model.
     * @tparam N The type representing nodes in the model.
     * @param localnodesinem A sequence of integers representing local nodes in the element model.
     */
    void definechildrenbuilder(seque<int> const &localnodesinem)
    {
        int etype = getnumber<E, TypeManager>();
        int ntype = getnumber<N, TypeManager>();
        int ctype = getnumber<C, TypeManager>();
        assert(getsize(thingmodels[ctype].symmetrygroups[ntype]) != 0);
        appendchildrenbuilder(thingmodels, etype, ctype, ntype, localnodesinem);
    }

    // define a symmetry group
    template<typename E, typename N>
    /**
     * Define a symmetry group for the given structure.
     *
     * @param sg A nested sequence of integers representing the symmetry group structure.
     */
    void definesymmetrygroup(seque<seque<int> > const &sg)
    {
        int etype = getnumber<E, TypeManager>();
        int ntype = getnumber<N, TypeManager>();
        appendsymmetrygroup(thingmodels, etype, ntype, sg);
    }

    // node/element insertion
    template<typename N, // the node
        typename... Args // the arguments you’d pass to N’s constructor
    >
    /**
     * @brief Appends a node of type N to the mesh with the given arguments.
     *
     * This method constructs an instance of the node type N using the provided
     * arguments and appends it to the internal mesh structure. The function
     * returns an identifier or index representing the position of the newly
     * added node in the mesh.
     *
     * @tparam N The type of the node to be appended.
     * @tparam Args Variadic template arguments passed to the constructor of the node.
     * @param args The arguments to construct the node of type N.
     * @return An integer representing the identifier or index of the appended node.
     */
    int appendnode(Args &&... args)
    {
        return appendnode<N>(mesh, N{std::forward<Args>(args)...});
    }

    // appendsnodets to a thing
    template<typename E, typename N>
    /**
     * Appends node numbers to a specified `thing` instance by associating
     * the provided node numbers sequence with a type derived from template parameters.
     *
     * @param res Reference to the `thing` object where the node numbers will be appended.
     * @param nodenumbers A constant reference to a sequence of integers representing the node numbers to append.
     *
     * The method uses template type information to determine and set the type number
     * in the `res` parameter and associates the given node numbers with the appropriate type.
     */
    void appendnodestoathing(thing &res, seque<int> const &nodenumbers)
    {
        int etype = getnumber<E, TypeManager>();
        int ntype = getnumber<N, TypeManager>();
        res.typenumber = etype;
        res.typesandnodes(ntype) = nodenumbers;
    }

    /**
     * Uploads a specified element (`thing`) to the system and recursively uploads all its child elements.
     *
     * This function handles the upload of a given `thing` object and iterates through all its child
     * elements, ensuring that all related data are uploaded in sequence.
     *
     * @param defined The `thing` object to be uploaded. It serves as the initial element whose
     *        data will be processed and uploaded, along with its child elements.
     */
    void uploadelement(thing const &defined)
    {
        uploadathing(lifemanager, defined, thingmodels[defined.typenumber]);
        seque<thing> allstuff =
                getallchildren(defined, thingmodels[defined.typenumber]);
        for (int e = 0; e < getsize(allstuff); ++e)
        {
            auto athing = allstuff[e];
            uploadathing(synthesized, athing, thingmodels[athing.typenumber]);
        }
    }

    template<typename E>
    /**
     * Retrieves the number of active elements of a specified type within the system.
     *
     * This method utilizes the `TypeManager` to determine the type identifier
     * for the specified element type `E`. It then accesses the `lifemanager` to find
     * the current number of active elements for the given type.
     *
     * Template Parameters:
     *   - E: The type of the element for which the active count will be retrieved.
     */
    void getactiveelements()
    {
        lifemanager.nactiveelements(getnumber<E, TypeManager>());
    }
};
