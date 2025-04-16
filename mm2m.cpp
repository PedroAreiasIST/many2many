#include "mm2m.hpp"

#include "godoftypes.hpp"
#include "o2m.hpp"
#include <algorithm>
#include <cassert>
#include <set>
#include <stack>
#include <utility>

// -----------------------------------------------------------------------------
// mm2m member functions and related helper routines
// -----------------------------------------------------------------------------

// Returns the number of nodes in a given element (of type elementType)
// for the specified node type (nodeType).
int mm2m::nnodes(int elementType, int element, int nodeType) const {
  // Basic index checks (assumes getsize() returns the number of elements)
  assert(elementType >= 0 && elementType < ntypes);
  assert(nodeType >= 0 && nodeType < ntypes);
  assert(element >= 0 &&
         element < getsize(operator()(elementType, nodeType).nfrome.lnods));
  return getsize(operator()(elementType, nodeType).nfrome.lnods[element]);
}

// Returns the number of elements (of the given element type)
// that are incident to a node (of type nodeType).
int mm2m::nelems(int nodeType, int node, int elementType) const {
  assert(nodeType >= 0 && nodeType < ntypes);
  assert(elementType >= 0 && elementType < ntypes);
  if (node < 0 ||
      node >= getsize(operator()(elementType, nodeType).efromn.lnods)) {
    return 0;
  } else
    return getsize(operator()(elementType, nodeType).efromn.lnods[node]);
}

// Resets the global list of marked nodes.
void resetmarked(mm2m &m) { erase(m.listofmarked); }

// Marks a node (identified by nodeType and its index) for erasure.
void marktoerase(mm2m &m, int nodeType, int node) {
  append(m.listofmarked, std::make_pair(nodeType, node));
}

void marktoeraserepeated(mm2m &m, int elementtype, int nodetype) {
  auto mm = m(elementtype, nodetype);
  auto order = getorder(mm);
  auto dupindices = getindicesofduplicates(mm.nfrome.lnods, order);
  for (int i = 0; i < getsize(dupindices); ++i)
    marktoerase(m, elementtype, dupindices[i]);
}

// -----------------------------------------------------------------------------
// Element–Node mapping routines
// -----------------------------------------------------------------------------

// Collects all (elementType, element) pairs that are incident to a given node.
// The node is identified by its type and index.
seque<std::pair<int, int>> getallelements(mm2m const &m,
                                          std::pair<int, int> const &node) {
  seque<std::pair<int, int>> ret;
  auto [nodeType, nodeNumber] = node;
  int totalElements = 0;
  // Compute total number of elements incident to the node.
  for (int elementType = 0; elementType < m.ntypes; ++elementType) {
    if (elementType != nodeType)
      totalElements += m.nelems(nodeType, nodeNumber, elementType);
  }
  setsize(ret, totalElements);
  int pos = 0;
  // Second pass: fill ret with pairs (elementType, element number)
  for (int elementType = 0; elementType < m.ntypes; ++elementType) {
    if (elementType != nodeType) {
      int numElems = m.nelems(nodeType, nodeNumber, elementType);
      for (int localelem = 0; localelem < numElems; ++localelem) {
        ret[pos++] = std::make_pair(
            elementType,
            m(elementType, nodeType).efromn.lnods[nodeNumber][localelem]);
      }
    }
  }
  setorderedandunique(ret);
  return ret;
}

seque<std::pair<int, int>> getallelements(mm2m const &m, int nodetype) {
  seque<std::pair<int, int>> ret;
  for (int node = 0; node < m(nodetype, nodetype).nfrome.nelem; ++node) {
    ret = getunion(ret, getallelements(m, std::make_pair(nodetype, node)));
  }
  setorderedandunique(ret);
  return ret;
}

// Collects all (nodeType, node) pairs associated with a given element.
// The element is specified by (elementType, element number).
seque<std::pair<int, int>> getallnodes(mm2m const &m,
                                       std::pair<int, int> const &element) {
  seque<std::pair<int, int>> ret;
  auto [elementType, elementNumber] = element;
  int totalNodes = 0;
  // First pass: count total nodes from all node types.
  for (int nodeType = 0; nodeType < m.ntypes; ++nodeType) {
    totalNodes += m.nnodes(elementType, elementNumber, nodeType);
  }
  setsize(ret, totalNodes);
  int pos = 0;
  // Second pass: fill ret with pairs (nodeType, node number).
  for (int nodeType = 0; nodeType < m.ntypes; ++nodeType) {
    int numberOfNodes = m.nnodes(elementType, elementNumber, nodeType);
    for (int localnode = 0; localnode < numberOfNodes; ++localnode) {
      ret[pos++] = std::make_pair(
          nodeType,
          m(elementType, nodeType).nfrome.lnods[elementNumber][localnode]);
    }
  }
  setorderedandunique(ret);
  return ret;
}

seque<std::pair<int, int>> getallnodes(mm2m const &m, int elementtype) {
  seque<std::pair<int, int>> ret;
  for (int element = 0; element < m(elementtype, element).nfrome.nelem;
       ++element) {
    ret = getunion(ret, getallnodes(m, std::make_pair(elementtype, element)));
  }
  setorderedandunique(ret);
  return ret;
}

// -----------------------------------------------------------------------------
// Depth-First Search and Related Operations
// -----------------------------------------------------------------------------

// Performs a depth-first search starting from a specified node (given as a
// pair) and returns all reachable (elementType, element) pairs. This helper is
// defined inside the hidden namespace.
seque<std::pair<int, int>>
hidden::depthfirstsearchfromanode(mm2m const &m,
                                  std::pair<int, int> const &node) {
  using P = std::pair<int, int>;
  seque<P> ret;
  std::set<P> visited;
  std::stack<P> stack;
  stack.push(node);
  while (!stack.empty()) {
    auto current = stack.top();
    stack.pop();
    if (visited.find(current) == visited.end()) {
      visited.insert(current);
      append(ret, current);
      seque<P> elements = getallelements(m, current);
      for (int i = 0; i < getsize(elements); ++i) {
        if (visited.find(elements[i]) == visited.end())
          stack.push(elements[i]);
      }
    }
  }
  setorderedandunique(ret);
  return ret;
}

// -----------------------------------------------------------------------------
// Insertion and Mapping Maintenance
// -----------------------------------------------------------------------------

// Sets the number of types and resizes the two-dimensional storage accordingly.
void setnumberoftypes(mm2m &m, int ntypes) {
  m.ntypes = ntypes;
  setsize(m.m, ntypes);
  for (int type = 0; type < ntypes; ++type) {
    setsize(m.m[type], ntypes);
  }
}

// Append an element for the given elementType/nodeType pairing.
// (This uses the o2m appendelement function defined elsewhere.)
int appendelement(mm2m &m, int elementType, int nodeType,
                  seque<int> const &nodes) {
  int newelement = appendelement(m(elementType, nodeType), nodes);
  return newelement;
}

// -----------------------------------------------------------------------------
// Compression and Remapping Routines
// -----------------------------------------------------------------------------

// Hidden compression function: For a given elementType, remap node indices
// according to the provided mapping arrays.
void hidden::compress(mm2m &m, int elementType,
                      seque<int> const &oldElementFromNew,
                      seque<int> const &newElementFromOld) {
  for (int nodeType = 0; nodeType < m.ntypes; ++nodeType) {
    compresselements(m(elementType, nodeType), oldElementFromNew);
  }
  // For each other elementType, update node indices accordingly.
  for (int otherElementType = 0; otherElementType < m.ntypes;
       ++otherElementType) {
    permutenodes(m(otherElementType, elementType), newElementFromOld);
  }
}

// Compresses (renumbers) the entire mm2m structure based on marked nodes.
// This involves (1) expanding the marked set via DFS, (2) building per-type
// node sets, (3) computing new mapping arrays, and (4) applying compression.
void compress(mm2m &m) {
  // Clean up the marked list.
  setorderedandunique(m.listofmarked);
  int markedSize = getsize(m.listofmarked);
  for (int counter = 0; counter < markedSize; ++counter) {
    seque<std::pair<int, int>> dfsResult =
        hidden::depthfirstsearchfromanode(m, m.listofmarked[counter]);
    for (int i = 0; i < getsize(dfsResult); ++i) {
      append(m.listofmarked, dfsResult[i]);
    }
  }
  setorderedandunique(m.listofmarked);

  // Build a per-type list of marked nodes.
  seque<seque<int>> nodes(m.ntypes);
  for (int counter = 0; counter < getsize(m.listofmarked); ++counter) {
    auto pair = m.listofmarked[counter];
    int type = pair.first;
    int node = pair.second;
    append(nodes[type], node);
  }

  // For each type, compute the maximum node index across relevant matrices,
  // build new mapping arrays, and compress.
  for (int type = 0; type < m.ntypes; ++type) {
    int nnmax = 0;

    for (int otherType = 0; otherType < m.ntypes; ++otherType) {
      nnmax = std::max(
          nnmax, m(type, otherType)
                     .nfrome.nelem); // gives number of elements of type type
      nnmax = std::max(nnmax, m(type, otherType).efromn.maxnode +
                                  1); // gives number of elements of type type
      nnmax = std::max(nnmax,
                       m(otherType, type)
                           .efromn.nelem); // gives number of nodes of type type
      nnmax = std::max(nnmax, m(otherType, type).nfrome.maxnode + 1);
    }

    seque<bool> isMarked(nnmax, false);
    for (int pos = 0; pos < getsize(nodes[type]); ++pos) {
      int node = nodes[type][pos];
      if (node >= 0 && node < nnmax)
        isMarked[node] = true;
    }
    setsize(m(type, type).nfrome, nnmax);
    setsize(m(type, type).efromn, nnmax);
    seque<int> sizes(nnmax, 1);
    setsizes(m(type, type).nfrome, sizes);
    setsizes(m(type, type).efromn, sizes);
    for (int i = 0; i < nnmax; ++i) {
      m(type, type).nfrome.lnods[i][0] = i;
      m(type, type).efromn.lnods[i][0] = i;
    }
    seque<int> oldFromNew(nnmax, -1);
    seque<int> newFromOld(nnmax, -1);
    int k = 0;
    for (int i = 0; i < nnmax; ++i) {
      if (!isMarked[i]) {
        oldFromNew[k] = i;
        newFromOld[i] = k;
        k++;
      }
    }
    setsize(oldFromNew, k);
    // Apply compression for the current type once the mapping is complete.
    hidden::compress(m, type, oldFromNew, newFromOld);
  }
  resetmarked(m);
}

// -----------------------------------------------------------------------------
// Type Dependency and Finalization Routines
// -----------------------------------------------------------------------------

// Build a topological order of the types based on inter-type dependencies.
// That is, if for a given pair (elementType, nodeType) the nfrome.nelem is
// nonzero, an edge is added from elementType to nodeType.
seque<int> typegettoporder(mm2m const &m) {
  o2m typeDeps;
  setsize(typeDeps, m.ntypes);
  for (int elementType = 0; elementType < m.ntypes; ++elementType) {
    for (int nodeType = 0; nodeType < m.ntypes; ++nodeType) {
      if (nodeType != elementType) {
        if (m(elementType, nodeType).nfrome.nelem != 0) {
          append(typeDeps.lnods[elementType], nodeType);
        }
      }
    }
  }
  return gettoporder(typeDeps);
}

// Retrieves elements from nodes for a particular pair of types.
seque<int> getselementsfromnodes(mm2m &matrix, int elementType, int nodeType,
                                 seque<int> const &nodes) {
  return getelementsdefinedbythesenodes(matrix(elementType, nodeType), nodes);
}
