// ======================================================================
// remesh.cpp — Simplex mesh splitting using the many2many framework
//
// Key m2m usage:
//   - OriginalMesh::edge_nodes is m2m: after synchronize(), edge lookup
//     by node pair uses getelementswithnodes(edge_nodes, {n1,n2}).
//   - RenewedMesh::elem_nodes is m2m: after synchronize(), face-neighbor
//     queries and smoothing stencils use the library's relational algebra.
//   - All element insertion via appendelement(m2m&, seque<int>).
//
// Pedro Areias / IST — GPLv3
// ======================================================================

#include "remesh.hpp"
#include <algorithm>
#include <cassert>
#include <iostream>
#include <map>
#include <stdexcept>

// =====================================================================
// Local tet-edge tables (0-based)
// =====================================================================

static constexpr int TET_EDGE[6][2] = {
    {0,1}, {1,2}, {0,2}, {2,3}, {0,3}, {1,3}
};
static constexpr int NODE_EDGES[4][3] = {
    {0,2,4}, {0,1,5}, {1,2,3}, {3,4,5}
};
static constexpr int TRI_EDGE[3][2] = {
    {0,1}, {1,2}, {2,0}
};
static constexpr int BAR_EDGE[1][2] = {
    {0,1}
};

// ------------------------------------------------------------------
// Helpers
// ------------------------------------------------------------------

static int edgeFromLocalNodes(int i1, int i2, int nedg, int const edgeTable[][2])
{
    for (int e = 0; e < nedg; ++e)
        if ((edgeTable[e][0] == i1 && edgeTable[e][1] == i2) ||
            (edgeTable[e][0] == i2 && edgeTable[e][1] == i1))
            return e;
    return -1;
}

static void opposingNodes(int ie, int &prev, int &post)
{
    static constexpr int OPP[6][2] = {
        {2,3}, {0,3}, {3,1}, {0,1}, {2,1}, {2,0}
    };
    prev = OPP[ie][0];
    post = OPP[ie][1];
}

static void nodesFrom2Nodes(int n1, int n2, int &o1, int &o2)
{
    int ie = edgeFromLocalNodes(n1, n2, 6, TET_EDGE);
    if (ie < 0) { o1 = o2 = -1; return; }
    opposingNodes(ie, o1, o2);
}

static int splitNode(int i1, int i2, std::array<int,6> const &midnodes)
{
    int ie = edgeFromLocalNodes(i1, i2, 6, TET_EDGE);
    return (ie >= 0) ? midnodes[ie] : 0;
}

// =====================================================================
// Edge relations — builds edge_nodes as m2m, then synchronizes
// =====================================================================

void buildEdgeRelations(OriginalMesh &mesh)
{
    auto nedgesForNnodes = [](int nn) -> int {
        switch(nn) { case 1: return 0; case 2: return 1; case 3: return 3; case 4: return 6; default: return 0; }
    };
    auto getEdgeTable = [](int nn) -> int const (*)[2] {
        switch(nn) { case 2: return BAR_EDGE; case 3: return TRI_EDGE; case 4: return TET_EDGE; default: return nullptr; }
    };

    // Discover unique edges via sorted-pair map
    std::map<std::pair<int,int>, int> pairToEdge;
    setsize(mesh.elem_edges, mesh.nel);
    int nextEdge = 0;

    for (int iel = 0; iel < mesh.nel; ++iel) {
        int nn = mesh.elem_nodes.nfrome.nnodes(iel);
        int ne = nedgesForNnodes(nn);
        auto et = getEdgeTable(nn);
        seque<int> edgeList;
        setsize(edgeList, ne);
        for (int le = 0; le < ne; ++le) {
            int gn1 = mesh.elem_nodes.nfrome[iel][et[le][0]];
            int gn2 = mesh.elem_nodes.nfrome[iel][et[le][1]];
            auto key = std::make_pair(std::min(gn1,gn2), std::max(gn1,gn2));
            auto it = pairToEdge.find(key);
            int eidx;
            if (it == pairToEdge.end()) { eidx = nextEdge++; pairToEdge[key] = eidx; }
            else { eidx = it->second; }
            edgeList[le] = eidx;
        }
        mesh.elem_edges.lnods[iel] = edgeList;
    }
    mesh.elem_edges.nelem = mesh.nel;
    mesh.elem_edges.maxnode = (nextEdge > 0) ? nextEdge - 1 : 0;
    mesh.nar = nextEdge;

    // Build edge_nodes as m2m: each "element" is an edge with 2 nodes
    setnumberofelements(mesh.edge_nodes, mesh.nar);
    for (auto &[key, idx] : pairToEdge)
        mesh.edge_nodes.nfrome.lnods[idx] = {key.first, key.second};
    mesh.edge_nodes.nfrome.nelem = mesh.nar;
    mesh.edge_nodes.nfrome.maxnode = mesh.nno - 1;
    mesh.edge_nodes.isupdated = false;

    // Synchronize: builds efromn = node→edges (via Tr), nodeloc, elementloc
    synchronize(mesh.edge_nodes);
}

// =====================================================================
// Mark edges — uses getelementswithnodes() on edge_nodes m2m
// =====================================================================

seque<int> markEdges(OriginalMesh &mesh)
{
    seque<int> mark(mesh.nar, 0);

    for (int i = 0; i < mesh.nmarkednodepairs; ++i) {
        int n1 = mesh.markednodepairs[i].first;
        int n2 = mesh.markednodepairs[i].second;
        if (n1 < 0 || n1 >= mesh.nno || n2 < 0 || n2 >= mesh.nno || n1 == n2)
            continue;

        // Find the edge "element" in edge_nodes that contains BOTH n1 and n2
        seque<int> edgeHits = getelementswithnodes(mesh.edge_nodes, seque<int>{n1, n2});
        if (getsize(edgeHits) > 0)
            mark[edgeHits[0]] = 1;
    }
    return mark;
}

// =====================================================================
// Create edge nodes
// =====================================================================

void createEdgeNodes(OriginalMesh const &mesh, RenewedMesh &newmesh, seque<int> &mark)
{
    int nnew = 0;
    for (int iar = 0; iar < mesh.nar; ++iar)
        if (mark[iar] != 0) ++nnew;

    newmesh.nno = mesh.nno + nnew;
    setsize(newmesh.parentnodes, newmesh.nno);
    for (int i = 0; i < mesh.nno; ++i)
        newmesh.parentnodes[i] = {i, i};

    int newIdx = mesh.nno;
    for (int iar = 0; iar < mesh.nar; ++iar) {
        if (mark[iar] != 0) {
            mark[iar] = newIdx;
            int n1 = mesh.edge_nodes.nfrome[iar][0];
            int n2 = mesh.edge_nodes.nfrome[iar][1];
            newmesh.parentnodes[newIdx] = {n1, n2};
            newIdx++;
        }
    }
    std::cout << "nno before=" << mesh.nno << "  nno after=" << newmesh.nno << std::endl;
}

// =====================================================================
// POINT elements
// =====================================================================

int countPoints(seque<int> const &mark, OriginalMesh const &mesh)
{
    // Build per-node point-element count using an o2m: node → point-elements
    // But we only need counts here, so a simple vector suffices
    seque<int> pelcount(mesh.nno, 0);
    int npoint = 0;
    for (int iel = 0; iel < mesh.nel; ++iel) {
        if (mesh.elem_nodes.nfrome.nnodes(iel) == 1) {
            pelcount[mesh.elem_nodes.nfrome[iel][0]]++;
            npoint++;
        }
    }
    for (int iar = 0; iar < mesh.nar; ++iar) {
        if (mark[iar] != 0) {
            int n1 = mesh.edge_nodes.nfrome[iar][0];
            int n2 = mesh.edge_nodes.nfrome[iar][1];
            npoint += pelcount[n1] + pelcount[n2];
        }
    }
    return npoint;
}

void splitPoints(seque<int> const &mark, OriginalMesh const &mesh, RenewedMesh &nm)
{
    // Build node→point-element map as o2m, then transpose for node-indexed lookup
    o2m pointElem;  // point-element-index → {node}
    seque<int> pointElemId; // parallel: which original element
    for (int iel = 0; iel < mesh.nel; ++iel) {
        if (mesh.elem_nodes.nfrome.nnodes(iel) == 1) {
            int idx = appendelement(pointElem, seque<int>{mesh.elem_nodes.nfrome[iel][0]});
            int sz = getsize(pointElemId);
            setsize(pointElemId, sz + 1);
            pointElemId[sz] = iel;
        }
    }
    o2m nodeToPoint = Tr(pointElem); // node → list of local-point-indices

    int ipoint = 0;

    // Copy existing point elements (via m2m appendelement)
    for (int iel = 0; iel < mesh.nel; ++iel) {
        if (mesh.elem_nodes.nfrome.nnodes(iel) == 1) {
            int ing = mesh.elem_nodes.nfrome[iel][0];
            appendelement(nm.elem_nodes, seque<int>{ing, iel});
            ipoint++;
        }
    }

    // New point elements for midpoint nodes
    for (int iar = 0; iar < mesh.nar; ++iar) {
        if (mark[iar] == 0) continue;
        int midNode = mark[iar];
        int n1 = mesh.edge_nodes.nfrome[iar][0];
        int n2 = mesh.edge_nodes.nfrome[iar][1];

        // POINTs from n1
        if (n1 < nodeToPoint.nelems()) {
            for (int k = 0; k < nodeToPoint.nnodes(n1); ++k) {
                int lp = nodeToPoint[n1][k];
                appendelement(nm.elem_nodes, seque<int>{midNode, pointElemId[lp]});
                ipoint++;
            }
        }
        // POINTs from n2
        if (n2 < nodeToPoint.nelems()) {
            for (int k = 0; k < nodeToPoint.nnodes(n2); ++k) {
                int lp = nodeToPoint[n2][k];
                appendelement(nm.elem_nodes, seque<int>{midNode, pointElemId[lp]});
                ipoint++;
            }
        }
    }

    if (ipoint != nm.npoint)
        throw std::runtime_error("POINT count mismatch: " + std::to_string(ipoint)
                                 + " vs " + std::to_string(nm.npoint));
}

// =====================================================================
// BAR2 elements
// =====================================================================

int countBars(seque<int> const &mark, OriginalMesh const &mesh)
{
    int nbar = 0;
    for (int iel = 0; iel < mesh.nel; ++iel) {
        if (mesh.elem_nodes.nfrome.nnodes(iel) == 2) {
            int ia = mesh.elem_edges[iel][0];
            nbar += (mark[ia] != 0) ? 2 : 1;
        }
    }
    return nbar;
}

void splitBars(seque<int> const &mark, OriginalMesh const &mesh, RenewedMesh &nm)
{
    for (int iel = 0; iel < mesh.nel; ++iel) {
        if (mesh.elem_nodes.nfrome.nnodes(iel) != 2) continue;
        int ia  = mesh.elem_edges[iel][0];
        int n1  = mesh.elem_nodes.nfrome[iel][0];
        int n2  = mesh.elem_nodes.nfrome[iel][1];
        int mid = mark[ia];
        if (mid == 0) {
            appendelement(nm.elem_nodes, seque<int>{n1, n2, iel});
        } else {
            appendelement(nm.elem_nodes, seque<int>{n1, mid, iel});
            appendelement(nm.elem_nodes, seque<int>{mid, n2, iel});
        }
    }
}

// =====================================================================
// TRIA3 elements
// =====================================================================

int countTriangles(seque<int> const &mark, OriginalMesh const &mesh)
{
    int ntri = 0;
    for (int iel = 0; iel < mesh.nel; ++iel) {
        if (mesh.elem_nodes.nfrome.nnodes(iel) != 3) continue;
        int kk = 1;
        for (int le = 0; le < 3; ++le)
            if (mark[mesh.elem_edges[iel][le]] != 0) kk++;
        ntri += kk;
    }
    return ntri;
}

void splitTriangles(seque<int> const &mark, OriginalMesh const &mesh, RenewedMesh &nm)
{
    auto emit = [&](int a, int b, int c, int parent) {
        appendelement(nm.elem_nodes, seque<int>{a, b, c, parent});
    };

    for (int iel = 0; iel < mesh.nel; ++iel) {
        if (mesh.elem_nodes.nfrome.nnodes(iel) != 3) continue;

        int nmarked = 0;
        std::array<int,3> mid{};
        for (int le = 0; le < 3; ++le) {
            mid[le] = mark[mesh.elem_edges[iel][le]];
            if (mid[le] != 0) nmarked++;
        }

        int v0 = mesh.elem_nodes.nfrome[iel][0];
        int v1 = mesh.elem_nodes.nfrome[iel][1];
        int v2 = mesh.elem_nodes.nfrome[iel][2];

        switch (nmarked) {
        case 0:
            emit(v0, v1, v2, iel);
            break;
        case 1:
            for (int le = 0; le < 3; ++le) {
                if (mid[le] == 0) continue;
                int ln1 = TRI_EDGE[le][0], ln2 = TRI_EDGE[le][1], ln3 = 3 - ln1 - ln2;
                int gn1 = mesh.elem_nodes.nfrome[iel][ln1];
                int gn3 = mesh.elem_nodes.nfrome[iel][ln2];
                int gn4 = mesh.elem_nodes.nfrome[iel][ln3];
                emit(gn1, mid[le], gn4, iel);
                emit(mid[le], gn3, gn4, iel);
                break;
            }
            break;
        case 2:
            for (int lv = 0; lv < 3; ++lv) {
                int e0 = lv, e1 = (lv + 2) % 3;
                if (mid[e0] != 0 && mid[e1] != 0) {
                    int la = (lv + 1) % 3, lb = (lv + 2) % 3;
                    int gA = mesh.elem_nodes.nfrome[iel][lv];
                    int gB = mesh.elem_nodes.nfrome[iel][la];
                    int gC = mesh.elem_nodes.nfrome[iel][lb];
                    int mAB = mid[e0], mAC = mid[e1];
                    if (mAB > mAC) {
                        emit(gA, mAB, gC, iel);
                        emit(mAB, mAC, gC, iel);
                    } else {
                        emit(gA, mAB, mAC, iel);
                        emit(gA, mAC, gC, iel);
                    }
                    emit(mAB, gB, mAC, iel);
                    break;
                }
            }
            break;
        case 3: {
            int m01 = mid[0], m12 = mid[1], m20 = mid[2];
            emit(v0, m01, m20, iel);
            emit(m01, v1, m12, iel);
            emit(m12, v2, m20, iel);
            emit(m01, m12, m20, iel);
            break;
        }
        }
    }
}

// =====================================================================
// Tet sub-splitting (faithful port of Fortran splittetwork0/1)
// =====================================================================

int splitTetCount(std::array<int,6> const &midnodes)
{
    int nmarked = 0;
    for (int i = 0; i < 6; ++i) if (midnodes[i] > 0) nmarked++;
    std::array<int,4> nm_{};
    for (int v = 0; v < 4; ++v)
        for (int j = 0; j < 3; ++j)
            if (midnodes[NODE_EDGES[v][j]] > 0) nm_[v]++;
    int mm = *std::max_element(nm_.begin(), nm_.end());

    switch (nmarked) {
    case 0: return 1;
    case 1: return 2;
    case 2: return (mm == 2) ? 3 : 4;
    case 3: {
        bool has3 = false, has0 = false;
        for (int v = 0; v < 4; ++v) { if (nm_[v]==3) has3=true; if (nm_[v]==0) has0=true; }
        return (has0 || has3) ? 4 : 5;
    }
    case 4: return 6;
    case 5: return 7;
    case 6: return 8;
    }
    return 1;
}

#define TET(a,b,c,d) do { tets[ntets++] = {a,b,c,d}; } while(0)

static void splitPyramid(int i1, int i2, int i3, int i4, int i5,
                         std::array<std::array<int,4>,8> &tets, int &ntets)
{
    if (std::max(i1,i3) > std::max(i2,i4)) { TET(i1,i2,i5,i3); TET(i1,i5,i4,i3); }
    else { TET(i1,i2,i5,i4); TET(i2,i3,i5,i4); }
}

static void splitPrism(int i1, int i2, int i3, int i4, int i5, int i6,
                       std::array<std::array<int,4>,8> &tets, int &ntets)
{
    bool s46 = std::max(i4,i6) > std::max(i3,i5);
    bool s13 = std::max(i1,i3) > std::max(i2,i4);
    bool s16 = std::max(i1,i6) > std::max(i2,i5);
    if (s46) {
        if (s13)      { TET(i1,i2,i6,i3); TET(i1,i6,i4,i3); TET(i1,i6,i5,i4); }
        else if (s16) { TET(i1,i2,i6,i4); TET(i1,i6,i5,i4); TET(i2,i6,i4,i3); }
        else          { TET(i1,i2,i5,i4); TET(i2,i3,i6,i4); TET(i2,i6,i5,i4); }
    } else {
        if (s13) {
            if (s16)  { TET(i1,i2,i6,i3); TET(i1,i3,i5,i4); TET(i1,i6,i5,i3); }
            else      { TET(i1,i3,i5,i4); TET(i2,i5,i1,i3); TET(i2,i6,i5,i3); }
        } else        { TET(i1,i2,i5,i4); TET(i2,i3,i5,i4); TET(i2,i6,i5,i3); }
    }
}

static void splitOcto(int i1, int i2, int i3, int i4, int i5, int i6,
                      std::array<std::array<int,4>,8> &tets, int &ntets)
{
    if (std::max(i3,i5) > std::max(i4,i6)) {
        TET(i1,i3,i4,i5); TET(i1,i3,i5,i6); TET(i3,i4,i5,i2); TET(i3,i5,i6,i2);
    } else {
        TET(i1,i4,i6,i3); TET(i1,i6,i4,i5); TET(i2,i4,i6,i5); TET(i2,i6,i4,i3);
    }
}

int splitTetBuild(std::array<int,4> const &nodes,
                  std::array<int,6> const &midnodes,
                  std::array<std::array<int,4>,8> &tets)
{
    int ntets = 0;
    int nmarked = 0;
    std::array<int,6> marked{};
    for (int i = 0; i < 6; ++i) if (midnodes[i] > 0) marked[nmarked++] = i;

    std::array<int,5> imarks{};
    std::array<int,12> jmarks{};
    imarks[0] = 0;
    for (int v = 0; v < 4; ++v) {
        imarks[v+1] = imarks[v];
        for (int j = 0; j < 3; ++j) {
            int ie = NODE_EDGES[v][j];
            if (midnodes[ie] > 0) { jmarks[imarks[v+1]] = ie; imarks[v+1]++; }
        }
    }

    auto SN = [&](int i1, int i2) -> int { return splitNode(i1, i2, midnodes); };
    auto N = [&](int lv) -> int { return nodes[lv]; };

    switch (nmarked) {
    case 0:
        TET(N(0),N(1),N(2),N(3));
        break;
    case 1: {
        int e = marked[0];
        int ln1 = TET_EDGE[e][0], ln2 = TET_EDGE[e][1], ln3, ln4;
        nodesFrom2Nodes(ln1, ln2, ln3, ln4);
        TET(N(ln3),N(ln4),midnodes[e],N(ln2));
        TET(N(ln4),N(ln3),midnodes[e],N(ln1));
        break;
    }
    case 2: {
        int mm = 0;
        for (int v = 0; v < 4; ++v) mm = std::max(mm, imarks[v+1]-imarks[v]);
        if (mm == 2) {
            int ln3 = -1, ln4 = -1;
            for (int v = 0; v < 4; ++v) {
                if (imarks[v+1]-imarks[v] == 0) ln3 = v;
                if (imarks[v+1]-imarks[v] == 2) ln4 = v;
            }
            int ln1, ln2; nodesFrom2Nodes(ln3, ln4, ln1, ln2);
            int n5 = SN(ln2, ln4), n6 = SN(ln1, ln4);
            splitPyramid(N(ln1), N(ln2), n5, n6, N(ln3), tets, ntets);
            TET(n6, n5, N(ln3), N(ln4));
        } else {
            int e0 = marked[0];
            int ln1 = TET_EDGE[e0][0], ln2 = TET_EDGE[e0][1], ln3, ln4;
            nodesFrom2Nodes(ln1, ln2, ln3, ln4);
            int n5 = SN(ln1, ln2), n6 = SN(ln3, ln4);
            TET(N(ln2),N(ln3),n5,n6); TET(N(ln2),n6,n5,N(ln4));
            TET(N(ln1),n5,n6,N(ln4)); TET(N(ln1),n5,N(ln3),n6);
        }
        break;
    }
    case 3: {
        int n4v = -1, n3v = -1;
        for (int v = 0; v < 4; ++v) {
            if (imarks[v+1]-imarks[v] == 3) n4v = v;
            if (imarks[v+1]-imarks[v] == 0) n3v = v;
        }
        if (n4v >= 0) {
            int ln4 = n4v, ln1 = iclock(4, ln4+2)-1, ln2, ln3;
            nodesFrom2Nodes(ln1, ln4, ln2, ln3);
            int n5 = SN(ln3,ln4), n6 = SN(ln1,ln4), n7 = SN(ln2,ln4);
            TET(N(ln4),n7,n6,n5);
            splitPrism(N(ln2),n7,n6,N(ln1),N(ln3),n5, tets, ntets);
        } else if (n3v >= 0) {
            int ln3 = n3v, ln4 = iclock(4, ln3+2)-1, ln1, ln2;
            nodesFrom2Nodes(ln3, ln4, ln1, ln2);
            int n5 = SN(ln1,ln4), n6 = SN(ln1,ln2), n7 = SN(ln2,ln4);
            TET(N(ln1),n6,N(ln3),n5); TET(N(ln2),N(ln3),n6,n7);
            TET(n6,N(ln3),n5,n7); TET(n5,n7,N(ln3),N(ln4));
        } else {
            int ln1=-1, ln2=-1, ln3=-1, ln4=-1;
            for (int v = 0; v < 4; ++v) {
                if (imarks[v+1]-imarks[v] == 1) {
                    int ie = jmarks[imarks[v]];
                    if (ln1 < 0) { ln1 = v; ln4 = (TET_EDGE[ie][0]==ln1) ? TET_EDGE[ie][1] : TET_EDGE[ie][0]; }
                    else         { ln2 = v; ln3 = (TET_EDGE[ie][0]==ln2) ? TET_EDGE[ie][1] : TET_EDGE[ie][0]; }
                }
            }
            int n5 = SN(ln1,ln4), n6 = SN(ln2,ln3), n7 = SN(ln3,ln4);
            splitPyramid(n6,n7,N(ln4),N(ln2),n5, tets, ntets);
            splitPyramid(N(ln3),N(ln1),n5,n7,n6, tets, ntets);
            TET(N(ln1),N(ln2),n6,n5);
        }
        break;
    }
    case 4: {
        int mm = 0;
        for (int v = 0; v < 4; ++v) mm = std::max(mm, imarks[v+1]-imarks[v]);
        if (mm == 3) {
            int ln3=-1, ln4=-1;
            for (int v = 0; v < 4; ++v) {
                if (imarks[v+1]-imarks[v]==1) ln3=v;
                if (imarks[v+1]-imarks[v]==3) ln4=v;
            }
            int ln1, ln2; nodesFrom2Nodes(ln3, ln4, ln1, ln2);
            int n5=SN(ln1,ln2), n6=SN(ln2,ln4), n7=SN(ln3,ln4), n8=SN(ln1,ln4);
            TET(n8,n6,n7,N(ln4)); TET(n7,n8,n5,n6);
            splitPyramid(N(ln2),N(ln3),n7,n6,n5, tets, ntets);
            splitPyramid(N(ln1),n8,n7,N(ln3),n5, tets, ntets);
        } else {
            int ie = -1;
            for (int i = 0; i < 6; ++i) if (midnodes[i]==0) { ie=i; break; }
            int ln1=TET_EDGE[ie][0], ln2=TET_EDGE[ie][1], ln3, ln4;
            nodesFrom2Nodes(ln1, ln2, ln3, ln4);
            int n5=SN(ln1,ln4), n6=SN(ln1,ln3), n7=SN(ln2,ln3), n8=SN(ln2,ln4);
            splitPrism(n8,n7,N(ln3),N(ln4),n5,n6, tets, ntets);
            splitPrism(N(ln1),N(ln2),n8,n5,n6,n7, tets, ntets);
        }
        break;
    }
    case 5: {
        int ie = -1;
        for (int i = 0; i < 6; ++i) if (midnodes[i]==0) { ie=i; break; }
        int ln1=TET_EDGE[ie][0], ln2=TET_EDGE[ie][1], ln3, ln4;
        nodesFrom2Nodes(ln1, ln2, ln3, ln4);
        int n5=SN(ln1,ln4), n6=SN(ln1,ln3), n7=SN(ln2,ln3), n8=SN(ln3,ln4), n9=SN(ln2,ln4);
        TET(n9,n8,n5,N(ln4)); TET(N(ln3),n6,n7,n8);
        splitPyramid(n6,n7,n9,n5,n8, tets, ntets);
        splitPrism(N(ln1),N(ln2),n9,n5,n6,n7, tets, ntets);
        break;
    }
    case 6: {
        int n5=SN(0,3), n6=SN(1,3), n7=SN(2,3), n8=SN(0,2), n9=SN(0,1), n10=SN(1,2);
        TET(N(0),n9,n8,n5); TET(N(1),n10,n9,n6); TET(N(2),n8,n10,n7); TET(n5,n6,n7,N(3));
        splitOcto(n8,n6,n7,n5,n9,n10, tets, ntets);
        break;
    }
    }
    return ntets;
}

#undef TET

// =====================================================================
// TETRA4 elements
// =====================================================================

int countTets(seque<int> const &mark, OriginalMesh const &mesh)
{
    int ntet = 0;
    for (int iel = 0; iel < mesh.nel; ++iel) {
        if (mesh.elem_nodes.nfrome.nnodes(iel) != 4) continue;
        std::array<int,6> mid{};
        for (int le = 0; le < 6; ++le) mid[le] = mark[mesh.elem_edges[iel][le]];
        ntet += splitTetCount(mid);
    }
    return ntet;
}

void splitTets(seque<int> const &mark, OriginalMesh const &mesh, RenewedMesh &nm)
{
    for (int iel = 0; iel < mesh.nel; ++iel) {
        if (mesh.elem_nodes.nfrome.nnodes(iel) != 4) continue;
        std::array<int,6> mid{};
        for (int le = 0; le < 6; ++le) mid[le] = mark[mesh.elem_edges[iel][le]];
        std::array<int,4> verts;
        for (int j = 0; j < 4; ++j) verts[j] = mesh.elem_nodes.nfrome[iel][j];
        std::array<std::array<int,4>,8> subtets{};
        int nsub = splitTetBuild(verts, mid, subtets);
        for (int s = 0; s < nsub; ++s)
            appendelement(nm.elem_nodes,
                seque<int>{subtets[s][0], subtets[s][1], subtets[s][2], subtets[s][3], iel});
    }
}

// =====================================================================
// Top-level split
// =====================================================================

void splitmesh(OriginalMesh &mesh, RenewedMesh &newmesh)
{
    std::cout << "Building edge relations..." << std::endl;
    buildEdgeRelations(mesh);

    std::cout << "Marking edges..." << std::endl;
    seque<int> mark = markEdges(mesh);

    std::cout << "Creating edge nodes..." << std::endl;
    createEdgeNodes(mesh, newmesh, mark);

    newmesh.npoint  = countPoints(mark, mesh);
    newmesh.nbar2   = countBars(mark, mesh);
    newmesh.ntria3  = countTriangles(mark, mesh);
    newmesh.ntetra4 = countTets(mark, mesh);
    newmesh.nel = newmesh.npoint + newmesh.nbar2 + newmesh.ntria3 + newmesh.ntetra4;

    std::cout << "New mesh: " << newmesh.npoint << " points, "
              << newmesh.nbar2 << " bars, "
              << newmesh.ntria3 << " tris, "
              << newmesh.ntetra4 << " tets" << std::endl;

    splitPoints(mark, mesh, newmesh);
    splitBars(mark, mesh, newmesh);
    splitTriangles(mark, mesh, newmesh);
    splitTets(mark, mesh, newmesh);

    newmesh.nel = newmesh.elem_nodes.nfrome.nelems();

    // Synchronize: builds node→element, enabling adjacency queries for quality improvement
    synchronize(newmesh.elem_nodes);

    std::cout << "Split complete. nel=" << newmesh.nel << std::endl;
}
