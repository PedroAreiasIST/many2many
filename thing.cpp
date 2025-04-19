#include "thing.hpp"

// Appends the provided symmetry group to the specified element type and node
// type
void appendsymmetrygroup(seque<thingmodel> &em, int elementtype, int nodetype,
                         seque<seque<int> > const &group)
{
    append(em(elementtype).symmetrygroups(nodetype), group);
}

// Appends a children builder for the given element type, child type, and node
// type. Assumes that symmetry groups for the element type have been defined.
void appendchildrenbuilder(seque<thingmodel> &em, int elementtype,
                           int childtype, int nodetype,
                           seque<int> const &localnodesinem)
{
    assert(getsize(em(elementtype).symmetrygroups) != 0);
    append(em(elementtype).childrenbuilders(childtype)(nodetype), localnodesinem);
}

// Sets the type number for a thing.
// The function asserts that the thing's type number has not already been
// assigned.
void settypenumberofathing(thing &e, int elementnumber)
{
    assert(e.typenumber == -1);
    e.typenumber = elementnumber;
}

// Appends the given sequence of nodes for a specified node type to a thing.
// The thing must already have a valid type number.
void appendnodesofonetype(thing &e, int nodetype, seque<int> const &nodes)
{
    assert(e.typenumber >= 0);
    std::cout << "nodetype=" << nodetype << std::endl;
    append(e.typesandnodes(nodetype), nodes);
}

// Builds children from a given thing using the provided models.
// For each child type and each node type under that child type, new children
// are generated based on the specified children builder information.
seque<thing> getallchildren(thing const &element,
                            seque<thingmodel> const &models)
{
    assert(element.typenumber >= 0);
    seque<thing> result;
    thingmodel model = models(element.typenumber);

    // Loop over possible child types.
    for (int childtype = 0; childtype < getsize(model.childrenbuilders);
         ++childtype)
    {
        // Only process if there is at least one children builder for this child
        // type.
        if (getsize(model.childrenbuilders[childtype]) != 0)
        {
            // Loop over each nodetype available for this child type.
            for (int nodetype = 0;
                 nodetype < getsize(model.childrenbuilders[childtype]); ++nodetype)
            {
                auto builderCollection = model.childrenbuilders[childtype][nodetype];
                // For each instance in the builder for the current nodetype, create a
                // new child.
                for (int i = 0; i < getsize(builderCollection); ++i)
                {
                    auto localnodes = builderCollection[i];
                    // Retrieve the nodes from the parent 'element' based on these
                    // indices.
                    seque<int> nodes = element.typesandnodes[nodetype](localnodes);
                    // Create a new child object.
                    thing child;
                    settypenumberofathing(child, childtype);
                    appendnodesofonetype(child, nodetype, nodes);
                    append(result, child);
                }
            }
        }
    }
    return result;
}

// Uploads a thing into an mm2m matrix.
// For each node type of the thing, compute the canonical form using the model's
// symmetry groups, then append the element to the matrix.
void uploadathing(mm2m &m, thing const &e, seque<thingmodel> const &models)
{
    for (int i = 0; i < getsize(e.typesandnodes); ++i)
    {
        auto canon = getcanonicalform(e.typesandnodes[i],
                                      models[e.typenumber].symmetrygroups[i]);
        appendelement(m, e.typenumber, i, canon);
    }
}

// Uploads all children of a thing into a separate mm2m matrix.
void uploadchildren(mm2m &mchildren, thing const &e,
                    seque<thingmodel> const &models)
{
    seque<thing> children = getallchildren(e, models);
    for (int i = 0; i < getsize(children); ++i)
        uploadathing(mchildren, children[i], models);
}

// Uploads a collection of things and their children into corresponding mm2m
// matrices.
void uploadallstuff(mm2m &m, mm2m &mchildren, seque<thing> const &things,
                    seque<thingmodel> const &models)
{
    for (int i = 0; i < getsize(things); ++i)
    {
        uploadathing(m, things[i], models);
        uploadchildren(mchildren, things[i], models);
    }
}
