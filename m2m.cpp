#include "m2m.hpp"
#include "o2m.hpp"
#include <cassert>
#ifdef _OPENMP
#include <omp.h>
#endif

namespace {
inline const hidden::o2m *select_o2m(const m2m &rel, bool transpose) {
  return transpose ? &rel.elementsfromnode : &rel.nodesfromelement;
}
inline hidden::o2m *select_o2m(m2m &rel, bool transpose) {
  return transpose ? &rel.elementsfromnode : &rel.nodesfromelement;
}
} // namespace

size_t m2m::nnodes(size_t element) {
  return getsize(nodesfromelement.lnods[element]);
}

size_t m2m::nelems(size_t node) {
  return getsize(elementsfromnode.lnods[node]);
}

void setnumberofelements(m2m &rel, size_t nelem) {
  setnumberofelements(rel.nodesfromelement, nelem);
}

void setnodesforelement(m2m &rel, size_t element, seque<size_t> const &nodes) {
  setnodesforelement(rel.nodesfromelement, element, nodes);
  rel.isupdated = false;
}

size_t appendelement(m2m &rel, seque<size_t> const &nodes) {
  size_t newel = hidden::appendelement(rel.nodesfromelement, nodes);
  rel.isupdated = false;
  return newel;
}

void multiplication(const m2m &rela, bool transposea, const m2m &relb,
                    bool transposeb, m2m &relc) {
  const hidden::o2m *a = select_o2m(rela, transposea);
  const hidden::o2m *b = select_o2m(relb, transposeb);
  hidden::multiplication(*a, *b, relc.nodesfromelement);
  relc.isupdated = false;
}

void addition(const m2m &rela, bool transposea, const m2m &relb,
              bool transposeb, m2m &relc) {
  const hidden::o2m *a = select_o2m(rela, transposea);
  const hidden::o2m *b = select_o2m(relb, transposeb);
  hidden::addition(*a, *b, relc.nodesfromelement);
  relc.isupdated = false;
}

void intersection(const m2m &rela, bool transposea, const m2m &relb,
                  bool transposeb, m2m &relc) {
  const hidden::o2m *a = select_o2m(rela, transposea);
  const hidden::o2m *b = select_o2m(relb, transposeb);
  hidden::intersection(*a, *b, relc.nodesfromelement);
  relc.isupdated = false;
}

void subtraction(const m2m &rela, bool transposea, const m2m &relb,
                 bool transposeb, m2m &relc) {
  const hidden::o2m *a = select_o2m(rela, transposea);
  const hidden::o2m *b = select_o2m(relb, transposeb);
  hidden::subtraction(*a, *b, relc.nodesfromelement);
  relc.isupdated = false;
}

void setallpointers(m2m &rel) {
  if (!rel.isupdated) {
    transpose(rel.nodesfromelement, rel.elementsfromnode);
    setsize(rel.nodelocation, rel.elementsfromnode.nelem);
    for (size_t node = 0; node < rel.elementsfromnode.nelem; ++node)
      setsize(rel.nodelocation[node],
              getsize(rel.elementsfromnode.lnods[node]));
    seque<size_t> nextlocalelementofnode(rel.elementsfromnode.nelem, 0);
    size_t nelem = rel.nodesfromelement.nelem;
    for (size_t element = 0; element < nelem; ++element) {
      const seque<size_t> &nodes = rel.nodesfromelement.lnods[element];
      for (size_t localnode = 0; localnode < getsize(nodes); ++localnode) {
        size_t node = nodes[localnode];
        rel.nodelocation[node][nextlocalelementofnode[node]++] = localnode;
      }
    }
    rel.isupdated = true;
  }
}

seque<size_t> getelementswithnodes(m2m const &rel, seque<size_t> const &nodes) {
  assert(rel.isupdated);
  seque<size_t> elems;
  if (getsize(nodes) == 0)
    return elems;
  elems = rel.elementsfromnode.lnods[nodes[0]];
  for (size_t i = 1; i < getsize(nodes); ++i)
    elems = getintersection(elems, rel.elementsfromnode.lnods[nodes[i]]);
  return elems;
}

seque<size_t> getelementsfromnodes(m2m const &rel, seque<size_t> const &nodes) {
  assert(rel.isupdated);
  seque<size_t> elems = getelementswithnodes(rel, nodes), ret;
  for (size_t i = 0; i < getsize(elems); ++i)
    if (getsize(rel.nodesfromelement.lnods[elems[i]]) == getsize(nodes))
      append(ret, elems[i]);
  return ret;
}

seque<size_t> getelementneighbours(m2m const &rel, size_t element) {
  assert(rel.isupdated);
  seque<size_t> neighbours;
  setsize(neighbours, 0);
  const seque<size_t> &elementNodes = rel.nodesfromelement.lnods[element];
  for (size_t i = 0; i < getsize(elementNodes); ++i) {
    size_t node = elementNodes[i];
    const seque<size_t> &nodeElements = rel.elementsfromnode.lnods[node];
    for (size_t j = 0; j < getsize(nodeElements); ++j) {
      size_t other = nodeElements[j];
      if (other != element)
        append(neighbours, other);
    }
  }
  setorderedandunique(neighbours);
  return neighbours;
}

seque<size_t> getnodeneighbours(m2m const &rel, size_t node) {
  assert(rel.isupdated);
  seque<size_t> neighbours;
  setsize(neighbours, 0);
  const seque<size_t> &elements = rel.elementsfromnode.lnods[node];
  for (size_t i = 0; i < getsize(elements); ++i) {
    size_t elem = elements[i];
    const seque<size_t> &nodes = rel.nodesfromelement.lnods[elem];
    for (size_t j = 0; j < getsize(nodes); ++j) {
      size_t other = nodes[j];
      if (other != node)
        append(neighbours, other);
    }
  }
  setorderedandunique(neighbours);
  return neighbours;
}

void indicesfromorder(m2m const &rel, const seque<size_t> &elementorder,
                      seque<size_t> &oldfromnew, seque<size_t> &newfromold) {
  indicesfromorder(rel.nodesfromelement, elementorder, oldfromnew, newfromold);
}

void compresselements(m2m &rel, seque<size_t> const &oldelementfromnew) {
  compresselements(rel.nodesfromelement, oldelementfromnew);
  setallpointers(rel);
}

void permutenodes(m2m &rel, seque<size_t> const &newnodefromold) {
  permutenodes(rel.nodesfromelement, newnodefromold);
  setallpointers(rel);
}

void getelementstoelements(m2m const &rel, m2m &elementstoelements) {
  assert(rel.isupdated);
  hidden::multiplication(rel.nodesfromelement, rel.elementsfromnode,
                         elementstoelements.nodesfromelement);
  elementstoelements.isupdated = false;
}

void getnodestonodes(m2m const &rel, m2m &nodestonodes) {
  assert(rel.isupdated);
  hidden::multiplication(rel.elementsfromnode, rel.nodesfromelement,
                         nodestonodes.nodesfromelement);
  nodestonodes.isupdated = false;
}

seque<size_t> lexiorder(m2m const &rel) {
  return lexiorder(rel.nodesfromelement);
}

seque<size_t> toporder(m2m const &rel) {
  return toporder(rel.nodesfromelement);
}

size_t getlocalnodeposition(m2m const &rel, size_t node, size_t localelement) {
  assert(rel.isupdated);
  return rel.nodelocation[node][localelement];
}
