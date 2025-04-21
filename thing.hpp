#ifndef RELATIONMANAGER_HPP
#define RELATIONMANAGER_HPP
#include "mm2m.hpp"
#include "seque.hpp"
#include "symmetries.hpp"

struct thingmodel
{
    seque<seque<seque<int> > >
    symmetrygroups; // [nodetype][combinationnumber][localnode]
    seque<seque<seque<seque<int> > > >
    childrenbuilders; // [childtype][nodetype][localnode]
};

PFR_FUNCTIONS_FOR(thingmodel)

struct thing
{
    int typenumber{-1};
    seque<seque<int> > typesandnodes; // [nodetype][localnode]->nodenumber
};

PFR_FUNCTIONS_FOR(thing)

// thing model
void appendsymmetrygroup(seque<thingmodel> &em, int elementtype, int nodetype,
                         seque<seque<int> > const &group);

void appendchildrenbuilder(seque<thingmodel> &em, int elementtype,
                           int childtype, int nodetype,
                           seque<int> const &localnodesinem);

// thing
void settypenumberofathing(thing &e, int elementnumber);

void appendelement(thing &e, int nodetype, seque<int> const &nodes);

seque<thing> getallchildren(thing const &element,
                            thingmodel const &models);

void uploadathing(mm2m &m, thing const &e, thingmodel const &model);


#endif // RELATIONMANAGER_HPP
