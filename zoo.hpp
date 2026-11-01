#pragma once
#include "mm2m.hpp"
#include <chrono>
#include <cstdio>
#include <cstdlib>
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
    int uploadelement(thing const &defined)
    {
        const int id = uploadathing(lifemanager, defined, thingmodels[defined.typenumber]);
        seque<thing> allstuff =
                getallchildren(defined, thingmodels[defined.typenumber]);
        for (int e = 0; e < getsize(allstuff); ++e)
        {
            auto athing = allstuff[e];
            const int cid = uploadathing(synthesized, athing, thingmodels[athing.typenumber]);
            // the child records its parent: row cid of the synthesized (child type, parent type) relation
            m2m &parents = synthesized(athing.typenumber, defined.typenumber);
            if (parents.nfrome.nelems() < cid + 1)
                setnumberofelements(parents, cid + 1);
            setrow(parents.nfrome, cid, {id});
            parents.isupdated = false;
        }
        return id;
    }

    /**
     * Bulk synthesis: applies the builders (E -> C over N) to every stored element of type E at
     * once, in parallel, and appends the distinct children to the synthesized relations with the
     * union of their parents. Equivalent to uploadelement() per element followed by
     * deduplicate<C, N>(), for a batch (the stored elements must already be uploaded, with
     * their children not yet synthesized). @return the number of distinct children appended.
     */
    template<typename E, typename C, typename N>
    int synthesize()
    {
        const int et = getnumber<E, TypeManager>();
        const int ct = getnumber<C, TypeManager>();
        const int nt = getnumber<N, TypeManager>();
        auto const &builders = thingmodels[et].childrenbuilders[ct][nt];
        auto const &group = thingmodels[ct].symmetrygroups[nt];
        const int nb = getsize(builders);
        o2m const &parents = lifemanager(et, nt).nfrome;
        const int ne = parents.nelems();
        if (nb == 0 || ne == 0)
            return 0;
        const bool timing = std::getenv("M2M_TIMING") != nullptr;
        auto tnow = [] { return std::chrono::duration<double>(std::chrono::steady_clock::now().time_since_epoch()).count(); };
        double tphase = tnow();
        auto lap = [&](const char *what) { if (timing) { const double t = tnow(); std::fprintf(stderr, "synthesize %s %.4f\n", what, t - tphase); tphase = t; } };
        // candidate children: one row per (element, builder), in canonical form
        o2m cand;
        const int arity = getsize(builders[0]);
        for (int b = 1; b < nb; ++b)
            if (getsize(builders[b]) != arity)
                throw std::runtime_error("synthesize: the builders of one child type must have the same arity");
        setsize(cand, ne * nb);
        setsizeexact(cand.data, static_cast<long long>(ne) * nb * arity);
        for (int i = 0; i <= ne * nb; ++i)
            cand.start[i] = i * arity;
#pragma omp parallel for schedule(static)
        for (int e = 0; e < ne; ++e)
        {
            crowview pn = parents[e];
            for (int b = 0; b < nb; ++b)
            {
                seque<int> nodes(arity);
                for (int k = 0; k < arity; ++k)
                    nodes[k] = pn[builders[b][k]];
                seque<int> canon = getcanonicalform(nodes, group);
                int *dst = cand.data.begin() + static_cast<long long>(e * nb + b) * arity;
                for (int k = 0; k < arity; ++k)
                    dst[k] = canon[k];
            }
        }
        cand.maxnode = parents.maxnode;
        lap("candidates");
        // distinct children and their parents
        seque<int> order = getorder(cand);
        lap("order");
        seque<int> childofcand, candofchild;
        indicesfromorder(cand, order, childofcand, candofchild);
        lap("indices");
        const int nchild = getsize(candofchild);
        const int ncand = getsize(order);
        m2m &rel = synthesized(ct, nt);
        m2m &par = synthesized(ct, et);
        const int first = rel.nfrome.nelems();
        // node lists of the children (packed copy of the unique rows), appended in bulk
        {
            o2m rows = getrows(cand, candofchild);
            appendelements(rel.nfrome, rows);
            rel.isupdated = false;
            rel.islocated = false;
        }
        lap("children");
        // parents: the candidates of one child are consecutive in the order (it is lexicographic
        // and the children are numbered along it), so child k owns the run runstart[k]..runstart[k+1)
        {
            if (par.nfrome.nelems() < first)
                setnumberofelements(par, first);
            seque<int> runstart(nchild + 1);
#pragma omp parallel for schedule(static)
            for (int j = 0; j < ncand; ++j)
                if (j == 0 || childofcand[order[j]] != childofcand[order[j - 1]])
                    runstart[childofcand[order[j]]] = j;
            runstart[nchild] = ncand;
            // parents of a child: the elements of its run, each once, increasing
            auto parentsof = [&](int k, int *out) {
                int n = 0;
                for (int j = runstart[k]; j < runstart[k + 1]; ++j)
                {
                    const int e = order[j] / nb;
                    int i = n;
                    while (i > 0 && out[i - 1] > e)
                        --i;
                    if (i > 0 && out[i - 1] == e)
                        continue;
                    for (int q = n; q > i; --q)
                        out[q] = out[q - 1];
                    out[i] = e;
                    ++n;
                }
                return n;
            };
            o2m parrows;
            setsize(parrows, nchild);
            seque<int> psize(nchild);
#pragma omp parallel
            {
                seque<int> tmp;
#pragma omp for schedule(static)
                for (int k = 0; k < nchild; ++k)
                {
                    setsize(tmp, runstart[k + 1] - runstart[k]);
                    psize[k] = parentsof(k, tmp.begin());
                }
            }
            long long total = 0;
            for (int k = 0; k < nchild; ++k)
            {
                parrows.start[k] = static_cast<int>(total);
                total += psize[k];
            }
            parrows.start[nchild] = static_cast<int>(total);
            setsizeexact(parrows.data, total);
#pragma omp parallel for schedule(static)
            for (int k = 0; k < nchild; ++k)
                parentsof(k, parrows.data.begin() + parrows.start[k]);
            parrows.maxnode = ne - 1;
            appendelements(par.nfrome, parrows);
            par.isupdated = false;
            par.islocated = false;
        }
        lap("parents");
        for (int t = 0; t < TypeManager::Size; ++t)
            if (t != ct && synthesized(ct, t).nfrome.nelems() < first + nchild)
                setnumberofelements(synthesized(ct, t), first + nchild);
        lap("resize");
        return nchild;
    }

    /** Synthesized children of type C with exactly one parent of type E (the boundary of the
     *  stored elements of type E), after deduplication or synthesize(). */
    template<typename C, typename E>
    seque<int> getboundary()
    {
        const int ct = getnumber<C, TypeManager>();
        const int et = getnumber<E, TypeManager>();
        o2m const &par = synthesized(ct, et).nfrome;
        seque<int> res;
        for (int c = 0; c < par.nelems(); ++c)
            if (par.nnodes(c) == 1)
                append(res, c);
        return res;
    }

    /**
     * Removes the repeated synthesized children of type C (same canonical node list of type N):
     * the first copy survives and inherits the parents of the copies that are erased.
     */
    template<typename C, typename N>
    void deduplicate()
    {
        const int ct = getnumber<C, TypeManager>();
        const int nt = getnumber<N, TypeManager>();
        m2m &rel = synthesized(ct, nt);
        seque<int> order = getorder(rel.nfrome);
        seque<int> repeated;
        int rep = -1;
        for (int i = 0; i < getsize(order); ++i)
        {
            const int e = order[i];
            if (rel.nfrome.nnodes(e) == 0)
                continue;
            if (rep >= 0 && rel.nfrome[e] == rel.nfrome[rep])
            {
                for (int t = 0; t < TypeManager::Size; ++t)
                {
                    if (t == nt || t == ct)
                        continue;
                    m2m &other = synthesized(ct, t);
                    if (e < other.nfrome.nelems() && rep < other.nfrome.nelems() && other.nfrome.nnodes(e) != 0)
                    {
                        seque<int> a = other.nfrome[rep], b = other.nfrome[e];
                        setordered(a);
                        setordered(b);
                        setrow(other.nfrome, rep, getunion(a, b));
                        other.isupdated = false;
                    }
                }
                append(repeated, e);
            } else
                rep = e;
        }
        eraseelements(synthesized, ct, repeated);
    }

    /** True when type pt declares builders for children of type ct (so synthesized(ct, pt) holds parent links). */
    bool isparenttype(int ct, int pt) const
    {
        auto const &builders = thingmodels[pt].childrenbuilders;
        if (ct >= getsize(builders))
            return false;
        for (int nt = 0; nt < getsize(builders[ct]); ++nt)
            if (getsize(builders[ct][nt]) != 0)
                return true;
        return false;
    }

    /** Marks a stored element for removal (its children lose it as a parent at compress()). */
    template<typename E>
    void removeelement(int element)
    {
        marktoerase(lifemanager, getnumber<E, TypeManager>(), element);
    }

    /** Marks a node for removal: the elements and children containing it go with it at compress(). */
    template<typename N>
    void removenode(int node)
    {
        marktoerase(lifemanager, getnumber<N, TypeManager>(), node);
        marktoerase(synthesized, getnumber<N, TypeManager>(), node);
    }

    /**
     * Compresses both matrices: the stored elements reached by the depth-first search from the
     * marked entities are erased; every synthesized child loses them as parents, and a child
     * left without parents is erased as well (a shared face survives while one of its
     * volumes does).
     */
    void compress()
    {
        seque<std::pair<int, int> > gone = getmarkedclosure(lifemanager);
        for (int i = 0; i < getsize(gone); ++i)
        {
            const int et = gone[i].first, id = gone[i].second;
            for (int ct = 0; ct < TypeManager::Size; ++ct)
            {
                if (ct == et)
                    continue; // the diagonal relation is the identity, not a parent link
                m2m &parents = synthesized(ct, et);
                if (parents.nfrome.nelems() == 0)
                    continue;
                synchronize(parents);
                if (id >= parents.efromn.nelems())
                    continue;
                seque<int> children = parents.efromn[id];
                for (int k = 0; k < getsize(children); ++k)
                {
                    const int child = children[k];
                    seque<int> row = parents.nfrome[child];
                    setordered(row);
                    seque<int> rest = getdifference(row, seque<int>{id});
                    setrow(parents.nfrome, child, rest);
                    parents.isupdated = false;
                    // a child is erased only when no parent of any type is left
                    bool hasparent = false;
                    for (int pt = 0; pt < TypeManager::Size && !hasparent; ++pt)
                    {
                        if (pt == ct)
                            continue;
                        m2m const &other = synthesized(ct, pt);
                        hasparent = child < other.nfrome.nelems() && other.nfrome.nnodes(child) != 0 &&
                                    isparenttype(ct, pt);
                    }
                    if (!hasparent)
                        marktoerase(synthesized, ct, child);
                }
            }
        }
        ::compress(lifemanager);
        ::compress(synthesized);
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
    int getactiveelements()
    {
        return lifemanager.nactiveelements(getnumber<E, TypeManager>());
    }
};
