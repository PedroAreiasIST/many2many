#include "thing.hpp"
void appendsymmetrygroup(seque<thingmodel> &em, int elementtype, int nodetype,
                         seque<seque<int>> const &group) {
  append(em(elementtype).symmetrygroups(nodetype), group);
}
void appendchildrenbuilder(seque<thingmodel> &em, int elementtype,
                           int childtype, int nodetype,
                           seque<int> const &localnodesinem) {
  assert(getsize(em(elementtype).symmetrygroups) != 0);
  append(em(elementtype).childrenbuilders(childtype)(nodetype), localnodesinem);
}
void settypenumberofathing(thing &e, int elementnumber) {
  assert(e.typenumber == -1);
  e.typenumber = elementnumber;
}
void appendnodesofonetype(thing &e, int nodetype, seque<int> const &nodes) {
  assert(e.typenumber >= 0);
  append(e.typesandnodes(nodetype), nodes);
  // e.typesandnodes(nodetype) = nodes;
}
seque<thing> getchildrenfromathing(thing const &element,
                                   seque<thingmodel> const &models) {
  assert(element.typenumber >= 0);
  seque<thing> result;
  thingmodel model = models(element.typenumber);
  for (int childtype = 0; childtype < getsize(model.childrenbuilders);
       ++childtype) {
    thing anewthing;
    if (getsize(model.childrenbuilders[childtype]) != 0) {
      settypenumberofathing(anewthing, childtype);
      for (int nodetype = 0;
           nodetype < getsize(model.childrenbuilders[childtype]); ++nodetype) {
        seque<int> nodes = element.typesandnodes[nodetype](
            model.childrenbuilders[childtype][nodetype]);
        appendnodesofonetype(anewthing, nodetype, nodes);
      }
      append(result, anewthing);
    }
  }
  return result;
}
seque<thing> getallchildren(thing const &element,
                            seque<thingmodel> const &models) {
  seque<thing> outer = getchildrenfromathing(element, models);
  seque<thing> result = outer;
  if (getsize(outer) == 0)
    return result;
  //  seque<thing> intermediate = outer;
  for (int ggg = 0; ggg < 1; ++ggg) {
    seque<thing> intermediate;
    for (int i = 0; i < getsize(outer); ++i) {
      append(intermediate, getchildrenfromathing(outer[i], models));
    }
    outer = intermediate;
    if (getsize(intermediate) != 0) {
      append(result, intermediate);
    } else
      break;
  }
  return result;
}
void uploadathing(mm2m &m, thing const &e, seque<thingmodel> const &models) {
  for (int i = 0; i < getsize(e.typesandnodes); ++i) {
    auto canon = getcanonicalform(e.typesandnodes[i],
                                  models[e.typenumber].symmetrygroups[i]);
    appendelement(m, e.typenumber, i, canon);
  }
}
void uploadchildren(mm2m &mchildren, thing const &e,
                    seque<thingmodel> const &models) {
  seque<thing> things = getallchildren(e, models);
  for (int i = 0; i < getsize(things); ++i)
    uploadathing(mchildren, things[i], models);
}
void uploadallstuff(mm2m &m, mm2m &mchildren, seque<thing> const &things,
                    seque<thingmodel> const &models) {
  for (int i = 0; i < getsize(things); ++i) {
    uploadathing(m, things[i], models);
    //  uploadchildren(mchildren, things[i], models);
  }
}