// ======================================================================
// meshquality.cpp — Post-split tet mesh quality improvement
//
// Uses m2m relational algebra:
//   - getelementswithnodes() for face-neighbor queries (boundary detection)
//   - getnodeneighbours()   for Laplacian smoothing stencil
//   - efromn (node→element) for iterating adjacent tets
//
// Pedro Areias / IST
// ======================================================================

#include "meshquality.hpp"
#include <algorithm>
#include <iostream>
#include <limits>

static constexpr int FACE_NODES[4][3] = {
    {1,2,3}, {0,2,3}, {0,3,1}, {0,1,2}
};

// ------------------------------------------------------------------
// Geometry
// ------------------------------------------------------------------

double signedVolumeTet(Vec3 const (&x)[4])
{
    Vec3 u = x[1]-x[0], v = x[2]-x[0], w = x[3]-x[0];
    return dot(w, cross(u, v)) / 6.0;
}

double insphereRadius(Vec3 const (&x)[4])
{
    double V = volumeTet(x), SA = 0.0;
    for (int f = 0; f < 4; ++f) {
        Vec3 u = x[FACE_NODES[f][1]] - x[FACE_NODES[f][0]];
        Vec3 v = x[FACE_NODES[f][2]] - x[FACE_NODES[f][0]];
        SA += 0.5 * norm3(cross(u, v));
    }
    return (SA > 0.0) ? 3.0*V/SA : 0.0;
}

double circumsphereRadius(Vec3 const (&x)[4])
{
    Vec3 a = x[1]-x[0], b = x[2]-x[0], c = x[3]-x[0];
    Vec3 cb = cross(b,c), ca = cross(c,a), ab = cross(a,b);
    double a2 = dot(a,a), b2 = dot(b,b), c2 = dot(c,c);
    Vec3 num = a2*cb + b2*ca + c2*ab;
    double V6 = dot(a, cb);
    return (std::abs(V6) > 1e-15) ? norm3(num)/(2.0*std::abs(V6))
                                   : std::numeric_limits<double>::max();
}

double tetQuality(Vec3 const (&x)[4])
{
    double rin = insphereRadius(x), rc = circumsphereRadius(x);
    return (rc > 0.0) ? 3.0*rin/rc : 0.0;
}

// ------------------------------------------------------------------
// Helpers: extract tet data from elem_nodes (m2m)
// ------------------------------------------------------------------

static bool isTet(RenewedMesh const &nm, int iel)
{
    return nm.elem_nodes.nfrome.nnodes(iel) == 5; // 4 nodes + 1 parent
}

static void getTetNodes(RenewedMesh const &nm, int iel, int nodes[4])
{
    for (int j = 0; j < 4; ++j)
        nodes[j] = nm.elem_nodes.nfrome[iel][j];
}

static void buildTetCoords(int const nodes[4], std::vector<Vec3> const &coords, Vec3 x[4])
{
    for (int j = 0; j < 4; ++j) x[j] = coords[nodes[j]];
}

// ------------------------------------------------------------------
// Validate mesh post-split
// ------------------------------------------------------------------

int validateMeshPostSplit(RenewedMesh &nm, std::vector<Vec3> const &coords)
{
    int ninverted = 0, ntet = 0, nfixed = 0;
    double qmin = 1e30, qmax = 0.0, qavg = 0.0;
    int nq01 = 0, nq02 = 0;

    for (int iel = 0; iel < nm.nel; ++iel) {
        if (!isTet(nm, iel)) continue;
        ntet++;
        int nodes[4]; getTetNodes(nm, iel, nodes);
        Vec3 x[4]; buildTetCoords(nodes, coords, x);
        double sv = signedVolumeTet(x);

        if (sv < 0.0) {
            auto &lnods = nm.elem_nodes.nfrome.lnods[iel];
            std::swap(lnods[2], lnods[3]);
            nm.elem_nodes.isupdated = false; // topology changed
            getTetNodes(nm, iel, nodes);
            buildTetCoords(nodes, coords, x);
            sv = signedVolumeTet(x);
            if (sv > 0.0) { nfixed++; }
            else if (std::abs(sv) < 1e-15) { ninverted++; }
            else { std::swap(lnods[2], lnods[3]); nm.elem_nodes.isupdated = false; ninverted++; }
        }

        if (std::abs(sv) > 1e-15) {
            double q = tetQuality(x);
            qmin = std::min(qmin, q); qmax = std::max(qmax, q); qavg += q;
            if (q < 0.1) nq01++; if (q < 0.2) nq02++;
        }
    }
    if (ntet > 0) qavg /= ntet;

    std::cout << "=== Post-split mesh validation ===" << std::endl;
    std::cout << "  Total tets:            " << ntet << std::endl;
    std::cout << "  Inverted (fixed swap): " << nfixed << std::endl;
    std::cout << "  Inverted (remaining):  " << ninverted << std::endl;
    std::cout << "  Quality min:           " << qmin << std::endl;
    std::cout << "  Quality max:           " << qmax << std::endl;
    std::cout << "  Quality avg:           " << qavg << std::endl;
    std::cout << "  Tets with q < 0.1:     " << nq01 << std::endl;
    std::cout << "  Tets with q < 0.2:     " << nq02 << std::endl;
    std::cout << "===================================" << std::endl;
    return ninverted;
}

// ------------------------------------------------------------------
// Detect boundary nodes via getelementswithnodes()
//
// A face is on the boundary if only ONE tet contains all 3 of its nodes.
// getelementswithnodes(elem_nodes, {f0,f1,f2}) returns all elements
// containing those 3 nodes; if count ≤ 1, it's a boundary face.
// ------------------------------------------------------------------

static std::vector<bool> detectBoundaryNodes(RenewedMesh &nm, int nno)
{
    // Ensure synchronized (efromn available)
    synchronize(nm.elem_nodes);

    std::vector<bool> isBoundary(nno, false);

    for (int iel = 0; iel < nm.nel; ++iel) {
        if (!isTet(nm, iel)) continue;
        int nodes[4]; getTetNodes(nm, iel, nodes);

        for (int f = 0; f < 4; ++f) {
            int f0 = nodes[FACE_NODES[f][0]];
            int f1 = nodes[FACE_NODES[f][1]];
            int f2 = nodes[FACE_NODES[f][2]];

            // Use m2m relational query: find all elements sharing these 3 nodes
            seque<int> shared = getelementswithnodes(nm.elem_nodes, seque<int>{f0, f1, f2});

            // If only this tet has all 3 → boundary face
            if (getsize(shared) <= 1) {
                isBoundary[f0] = true;
                isBoundary[f1] = true;
                isBoundary[f2] = true;
            }
        }
    }
    return isBoundary;
}

// ------------------------------------------------------------------
// Laplacian smoothing using getnodeneighbours() and efromn
//
// getnodeneighbours(m2m, node) returns all nodes connected to `node`
// through shared elements — exactly the Laplacian stencil.
//
// efromn[node] gives all elements touching `node` — used to check
// quality of adjacent tets before/after a tentative move.
// ------------------------------------------------------------------

static int smoothInteriorNodes(RenewedMesh &nm,
                               std::vector<Vec3> &coords,
                               std::vector<bool> const &isBoundary,
                               int nsmooth)
{
    synchronize(nm.elem_nodes);
    int nno = (int)coords.size();
    int nmovedTotal = 0;

    for (int iter = 0; iter < nsmooth; ++iter) {
        int nmoved = 0;

        for (int ino = 0; ino < nno; ++ino) {
            if (isBoundary[ino]) continue;

            // Get Laplacian stencil via m2m relational query
            seque<int> neighbors = getnodeneighbours(nm.elem_nodes, ino);
            if (getsize(neighbors) == 0) continue;

            Vec3 xold = coords[ino];
            Vec3 xnew = {0,0,0};
            for (int k = 0; k < getsize(neighbors); ++k)
                xnew = xnew + coords[neighbors[k]];
            double inv = 1.0 / getsize(neighbors);
            xnew = {xnew[0]*inv, xnew[1]*inv, xnew[2]*inv};

            // Under-relaxation 50%
            xnew = 0.5*xold + 0.5*xnew;
            if (norm3(xnew - xold) < 1e-14) continue;

            // Min quality of adjacent tets BEFORE move (via efromn)
            double qOldMin = 1e30;
            seque<int> const &adjElems = nm.elem_nodes.efromn[ino];
            for (int k = 0; k < getsize(adjElems); ++k) {
                int jel = adjElems[k];
                if (!isTet(nm, jel)) continue;
                int nodes[4]; getTetNodes(nm, jel, nodes);
                Vec3 x[4]; buildTetCoords(nodes, coords, x);
                qOldMin = std::min(qOldMin, tetQuality(x));
            }

            // Tentative move
            coords[ino] = xnew;

            // Check adjacent tets: no inversion, quality improves
            bool ok = true;
            double qNewMin = 1e30;
            for (int k = 0; k < getsize(adjElems); ++k) {
                int jel = adjElems[k];
                if (!isTet(nm, jel)) continue;
                int nodes[4]; getTetNodes(nm, jel, nodes);
                Vec3 x[4]; buildTetCoords(nodes, coords, x);
                if (signedVolumeTet(x) <= 0.0) { ok = false; break; }
                qNewMin = std::min(qNewMin, tetQuality(x));
            }

            if (ok && qNewMin > qOldMin) nmoved++;
            else coords[ino] = xold; // revert
        }

        nmovedTotal += nmoved;
        if (nmoved == 0) break;
    }
    return nmovedTotal;
}

// ------------------------------------------------------------------
// Full improvement pipeline
// ------------------------------------------------------------------

void improveMeshQuality(RenewedMesh &nm, std::vector<Vec3> &coords, int maxSmoothIter)
{
    std::cout << "\n*** Mesh quality improvement ***\n" << std::endl;

    std::cout << "Step 1: Validating orientations..." << std::endl;
    int ninverted = validateMeshPostSplit(nm, coords);
    if (ninverted > 0)
        std::cout << "WARNING: " << ninverted << " tets remain inverted." << std::endl;

    // Re-synchronize after possible orientation swaps in validation
    synchronize(nm.elem_nodes);

    std::cout << "Step 2: Detecting boundary nodes (via getelementswithnodes)..." << std::endl;
    auto isBoundary = detectBoundaryNodes(nm, (int)coords.size());
    int nBnd = 0;
    for (auto b : isBoundary) if (b) nBnd++;
    std::cout << "  Boundary nodes: " << nBnd << " / " << coords.size() << std::endl;

    std::cout << "Step 3: Laplacian smoothing (via getnodeneighbours, max "
              << maxSmoothIter << " iterations)..." << std::endl;
    int nmoved = smoothInteriorNodes(nm, coords, isBoundary, maxSmoothIter);
    std::cout << "  Total node moves accepted: " << nmoved << std::endl;

    std::cout << "\nStep 4: Final validation..." << std::endl;
    validateMeshPostSplit(nm, coords);
    std::cout << "\n*** Mesh quality improvement complete ***\n" << std::endl;
}
