// Bulk synthesis with the element catalogue: the running example (counts and parents must equal
// the incremental path), an H8 grid (face and edge counts, boundary) and the throughput at scale.
#include "catalogue.hpp"
#include "graph.hpp"
#include "zoo.hpp"
#include <chrono>
#include <cstdio>
#include <iostream>

struct isanelement {}; struct node {}; struct edge {}; struct tri {}; struct quad {}; struct tet {}; struct wedge {}; struct hex {};
using MeshTypes = typseque<isanelement, node, edge, tri, quad, tet, wedge, hex>;
static int fails = 0;
#define CHECK(c, msg) do { if (!(c)) { ++fails; std::printf("FAIL: %s\n", msg); } } while (0)
static double now() { return std::chrono::duration<double>(std::chrono::steady_clock::now().time_since_epoch()).count(); }

int main(int argc, char **argv)
{
    using Z = zoo<MeshTypes>;
    using TM = Z::TypeManager;
    const int ntype = getnumber<node, TM>(), etype = getnumber<edge, TM>(), ttype = getnumber<tri, TM>(), qtype = getnumber<quad, TM>();
    // --- running example through the catalogue and bulk synthesis
    {
        Z g;
        g.definesymmetrygroup<isanelement, isanelement>({{}});
        g.definesymmetrygroup<node, node>({{}});
        declare<edge, node>(g, catalogue::bar2());
        declare<tri, node, void, void, edge>(g, catalogue::tri3());
        declare<quad, node, void, void, edge>(g, catalogue::quad4());
        declare<tet, node, tri, void, edge>(g, catalogue::tet4());
        declare<wedge, node, tri, quad, edge>(g, catalogue::wedge6());
        g.definesymmetrygroup<edge, isanelement>({{0}});
        g.definesymmetrygroup<tri, isanelement>({{0}});
        g.definesymmetrygroup<quad, isanelement>({{0}});
        g.definesymmetrygroup<tet, isanelement>({{0}});
        g.definesymmetrygroup<wedge, isanelement>({{0}});
        // stored elements only (uploadathing on lifemanager), then bulk synthesis
        thing t0; g.appendnodestoathing<tet, node>(t0, {4, 2, 6, 0}); g.appendnodestoathing<tet, isanelement>(t0, {0});
        thing e1; g.appendnodestoathing<edge, node>(e1, {3, 0}); g.appendnodestoathing<edge, isanelement>(e1, {0});
        thing e2; g.appendnodestoathing<edge, node>(e2, {3, 1}); g.appendnodestoathing<edge, isanelement>(e2, {0});
        thing w3; g.appendnodestoathing<wedge, node>(w3, {6, 3, 5, 1, 2, 4}); g.appendnodestoathing<wedge, isanelement>(w3, {0});
        for (thing *t: {&t0, &e1, &e2, &w3}) uploadathing(g.lifemanager, *t, g.thingmodels[t->typenumber]);
        const int ntri = g.synthesize<tet, tri, node>() + g.synthesize<wedge, tri, node>();
        const int nquad = g.synthesize<wedge, quad, node>();
        const int nedge = g.synthesize<tet, edge, node>() + g.synthesize<wedge, edge, node>();
        // the two synthesize calls of a child type do not deduplicate across parent types: merge
        g.deduplicate<tri, node>(); g.deduplicate<edge, node>();
        int tri_active = 0, edge_active = 0;
        for (int c = 0; c < g.synthesized(ttype, ntype).nfrome.nelems(); ++c) tri_active += g.synthesized(ttype, ntype).nfrome.nnodes(c) != 0;
        for (int c = 0; c < g.synthesized(etype, ntype).nfrome.nelems(); ++c) edge_active += g.synthesized(etype, ntype).nfrome.nnodes(c) != 0;
        std::printf("running example (bulk): %d tri (%d active), %d quad, %d edges (%d active)\n", ntri, tri_active, nquad, nedge, edge_active);
        CHECK(tri_active == 6 && nquad == 3 && edge_active == 14, "running example counts 6/3/14");
        // the shared edge (2,4) has two parents (tet 0 and wedge 0)
        auto hits = getelementsfromnodes(g.synthesized, etype, ntype, {2, 4});
        CHECK(getsize(hits) == 1, "one edge (2,4)");
        const int tetp = getnumber<tet, TM>(), wedp = getnumber<wedge, TM>();
        CHECK(g.synthesized(etype, tetp).nfrome.nnodes(hits[0]) == 1 && g.synthesized(etype, wedp).nfrome.nnodes(hits[0]) == 1, "(2,4) has a tet parent and a wedge parent");
        // boundary of the tetrahedron: all 4 faces (only one tet)
        CHECK(getsize(g.getboundary<tri, tet>()) == 4, "4 boundary faces of the lone tet");
    }
    // --- H8 grid: faces, edges, boundary
    const int nee = argc > 1 ? std::atoi(argv[1]) : 6;
    {
        Z g;
        g.definesymmetrygroup<node, node>({{}});
        g.definesymmetrygroup<isanelement, isanelement>({{}});
        g.definesymmetrygroup<quad, node>(catalogue::dihedral(4)); // faces are seen from both sides
        declare<edge, node>(g, catalogue::bar2());
        declare<hex, node, void, quad, edge>(g, catalogue::hex8());
        const int s1 = nee + 1, s2 = s1 * s1;
        const double t0 = now();
        m2m &st = g.lifemanager(getnumber<hex, TM>(), ntype);
        reserve(st.nfrome, nee * nee * nee, 8L * nee * nee * nee);
        for (int iex = 0; iex < nee; ++iex)
            for (int iey = 0; iey < nee; ++iey)
                for (int iez = 0; iez < nee; ++iez)
                {
                    const int b = iex + iey * s1 + iez * s2;
                    int nodes[8] = {b, b + 1, b + 1 + s1, b + s1, b + s2, b + 1 + s2, b + 1 + s1 + s2, b + s1 + s2};
                    appendelement(st.nfrome, crowview(nodes, 8));   // already canonical for the hex group? not necessarily: canonicalize
                }
        const double t1 = now();
        const int nq = g.synthesize<hex, quad, node>();
        const double t2 = now();
        const int ne = g.synthesize<hex, edge, node>();
        const double t3 = now();
        seque<int> bnd = g.getboundary<quad, hex>();
        const double t4 = now();
        const long nel = 1L * nee * nee * nee;
        std::printf("H8 %d^3: %ld elements, %d faces (expected %d), %d edges (expected %d), boundary %d (expected %d)\n", nee, nel, nq,
                    3 * nee * nee * (nee + 1), ne, 3 * (nee + 1) * (nee + 1) * nee, getsize(bnd), 6 * nee * nee);
        std::printf("TIMER upload %.3f faces %.3f edges %.3f boundary %.3f s  (%.2f M faces/s)\n", t1 - t0, t2 - t1, t3 - t2, t4 - t3, nq / (t2 - t1) / 1e6);
        CHECK(nq == 3 * nee * nee * (nee + 1), "face count");
        CHECK(ne == 3 * (nee + 1) * (nee + 1) * nee, "edge count");
        CHECK(getsize(bnd) == 6 * nee * nee, "boundary face count");
    }
    // the lex-min over the group (fast path) agrees with the backtracking search for large groups
    {
        seque<seque<int> > big = catalogue::hex8().group;
        seque<seque<int> > bigger = big;
        for (int i = 0; i < 9; ++i) append(bigger, big[i]); // 33 entries: forces the backtracking path (duplicates are harmless)
        // node lists have distinct entries (the uniqueness rule); with repeated labels the two
        // searches may return different images
        unsigned x = 12345;
        bool same = true;
        for (int t = 0; t < 2000 && same; ++t)
        {
            seque<int> labels(8);
            for (int k = 0; k < 8; ++k)
            {
                bool fresh;
                do { x = x * 1103515245u + 12345u; labels[k] = (int) ((x >> 8) % 50); fresh = true;
                     for (int j = 0; j < k; ++j) if (labels[j] == labels[k]) fresh = false; } while (!fresh);
            }
            same = getcanonicalform(labels, big) == getcanonicalform(labels, bigger);
        }
        CHECK(same, "lex-min fast path equals the backtracking canonical form");
    }
    std::printf(fails ? "%d FAILURES\n" : "all synthesis checks passed\n", fails);
    return fails != 0;
}
