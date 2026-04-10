#ifndef REMESH_HPP
#define REMESH_HPP
// ======================================================================
// remesh.hpp — Simplex mesh splitting using the many2many framework
//
// All connectivity uses m2m (bidirectional) or o2m relations.
// After synchronize(), the reverse mappings and adjacency queries
// (getelementswithnodes, getnodeneighbours, getelementneighbours)
// become available — replacing hand-coded CSR traversals.
//
// Pedro Areias / IST — GPLv3
// ======================================================================

#include "m2m.hpp"
#include "o2m.hpp"
#include "seque.hpp"
#include <array>
#include <string>
#include <utility>

// ------------------------------------------------------------------
// Mesh data structures
// ------------------------------------------------------------------

struct OriginalMesh
{
    int nno{0};
    int nel{0};
    int nar{0};

    /// Element↔node.  After synchronize():
    ///   nfrome  = element → nodes
    ///   efromn  = node → elements   (via Tr)
    m2m elem_nodes;

    /// Edge↔node.  After synchronize():
    ///   nfrome  = edge → 2 nodes
    ///   efromn  = node → edges      (via Tr)
    m2m edge_nodes;

    /// Element → local-edge-indices (one-directional).
    o2m elem_edges;

    int nmarkednodepairs{0};
    seque<std::pair<int,int>> markednodepairs;
};

struct RenewedMesh
{
    int nno{0};
    int nel{0};
    int npoint{0}, nbar2{0}, ntria3{0}, ntetra4{0};

    /// Element↔node.  After synchronize():
    ///   - getelementswithnodes(elem_nodes, {a,b,c}) finds face neighbors
    ///   - getnodeneighbours(elem_nodes, node)       gives Laplacian stencil
    ///   - getelementneighbours(elem_nodes, iel)     gives element adjacency
    /// Last entry per element = parent element (0-based).
    m2m elem_nodes;

    /// parentnodes[newnode] = {parent1, parent2} (0-based).
    seque<std::pair<int,int>> parentnodes;
};

// ------------------------------------------------------------------
// API
// ------------------------------------------------------------------

void buildEdgeRelations(OriginalMesh &mesh);
seque<int> markEdges(OriginalMesh &mesh);
void createEdgeNodes(OriginalMesh const &mesh, RenewedMesh &newmesh, seque<int> &mark);

int  countPoints   (seque<int> const &mark, OriginalMesh const &mesh);
int  countBars     (seque<int> const &mark, OriginalMesh const &mesh);
int  countTriangles(seque<int> const &mark, OriginalMesh const &mesh);
int  countTets     (seque<int> const &mark, OriginalMesh const &mesh);

void splitPoints   (seque<int> const &mark, OriginalMesh const &mesh, RenewedMesh &nm);
void splitBars     (seque<int> const &mark, OriginalMesh const &mesh, RenewedMesh &nm);
void splitTriangles(seque<int> const &mark, OriginalMesh const &mesh, RenewedMesh &nm);
void splitTets     (seque<int> const &mark, OriginalMesh const &mesh, RenewedMesh &nm);

void splitmesh(OriginalMesh &mesh, RenewedMesh &newmesh);

int splitTetCount(std::array<int,6> const &midnodes);
int splitTetBuild(std::array<int,4> const &nodes,
                  std::array<int,6> const &midnodes,
                  std::array<std::array<int,4>,8> &tets);

inline int iclock(int n, int i)
{
    int r = ((i - 1) % n);
    if (r < 0) r += n;
    return r + 1;
}

#endif
