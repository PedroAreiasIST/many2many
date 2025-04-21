#pragma once
#include "mm2m.hpp"
#include "superstruct.hpp"
#include "typseque.hpp"
#include "basics.hpp"
#include "thing.hpp"

template<typename TypeList>
struct zoo
{
    using TypeManager = typsequetostructtype<TypeList, superstruct>;
    mm2m lifemanager;
    mm2m synthesized;
    TypeManager mesh;
    seque<thingmodel> thingmodels;

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
                setsize(thingmodels[type].childrenbuilders[othertype], TypeManager::Size);
            }
        }
    }

    // model definition
    template<typename E, typename C, typename N>
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
    void definesymmetrygroup(seque<seque<int> > const &sg)
    {
        int etype = getnumber<E, TypeManager>();
        int ntype = getnumber<N, TypeManager>();
        appendsymmetrygroup(thingmodels, etype, ntype, sg);
    }

    // node/element insertion
    template<
        typename N, // the node
        typename... Args // the arguments you’d pass to N’s constructor
    >
    int appendnode(Args &&... args)
    {
        return appendnode<N>(mesh, N{std::forward<Args>(args)...});
    }

    // appendsnodets to a thing
    template<typename E, typename N>
    void appendnodestoathing(thing &res, seque<int> const &nodenumbers)
    {
        int etype = getnumber<E, TypeManager>();
        int ntype = getnumber<N, TypeManager>();
        res.typenumber = etype;
        res.typesandnodes(ntype) = nodenumbers;
    }

    void uploadelement(thing const &defined)
    {
        uploadathing(lifemanager, defined, thingmodels[defined.typenumber]);
        seque<thing> allstuff = getallchildren(defined, thingmodels[defined.typenumber]);
        for (int e = 0; e < getsize(allstuff); ++e)
        {
            auto athing = allstuff[e];
            uploadathing(synthesized, athing, thingmodels[athing.typenumber]);
        }
    }

    template<typename E>
    void getactiveelements()
    {
        lifemanager.nactiveelements(getnumber<E, TypeManager>());
    }
};


