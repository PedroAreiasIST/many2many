// The running example of the revised paper (4 generalized elements, 7 nodes),
// built through the zoo: counts of stored and synthesized entities before and
// after the removal of node 3.
#include "basics.hpp"
#include "mm2m.hpp"
#include "symmetries.hpp"
#include "thing.hpp"
#include "typseque.hpp"
#include "zoo.hpp"
#include <iostream>

struct isanelement {};
struct node {};
struct edge {};
struct tri {};
struct quad {};
struct tet {};
struct wedge {};

using MeshTypes = typseque<isanelement, node, edge, tri, quad, tet, wedge>;

template <typename Z>
static void report(Z &garden, const char *label)
{
    using TM = typename Z::TypeManager;
    const char *names[] = {"isanelement", "node", "edge", "tri", "quad", "tet", "wedge"};
    const int ntype = getnumber<node, TM>();
    std::cout << "== " << label << " ==\n";
    for (int t = 0; t < TM::Size; ++t)
    {
        auto &a = garden.lifemanager(t, ntype).nfrome;
        auto &b = garden.synthesized(t, ntype).nfrome;
        std::cout << names[t] << ": stored rows " << a.nelems() << " | synthesized rows " << b.nelems() << "\n";
        for (int e = 0; e < b.nelems(); ++e)
        {
            std::cout << "   synth " << names[t] << " " << e << ": {";
            for (int k = 0; k < b.nnodes(e); ++k) std::cout << (k ? "," : "") << b[e][k];
            std::cout << "}";
            // parents of the child, one relation per parent type
            for (int pt = 0; pt < TM::Size; ++pt)
            {
                auto &pr = garden.synthesized(t, pt).nfrome;
                if (pt == ntype || e >= pr.nelems() || pr.nnodes(e) == 0) continue;
                std::cout << "  parents(" << names[pt] << ")={";
                for (int k = 0; k < pr.nnodes(e); ++k) std::cout << (k ? "," : "") << pr[e][k];
                std::cout << "}";
            }
            std::cout << "\n";
        }
        for (int e = 0; e < a.nelems(); ++e)
        {
            std::cout << "   stored " << names[t] << " " << e << ": {";
            for (int k = 0; k < a.nnodes(e); ++k) std::cout << (k ? "," : "") << a[e][k];
            std::cout << "}\n";
        }
    }
}

int main()
{
    const seque<seque<int> > emptysymm{{}};
    const seque<seque<int> > e2symm{{0, 1}, {1, 0}};
    const seque<seque<int> > t3sym{{0, 1, 2}, {1, 2, 0}, {2, 0, 1}};
    const seque<seque<int> > q4sym{{0, 1, 2, 3}, {1, 2, 3, 0}, {2, 3, 0, 1}, {3, 0, 1, 2}};
    const seque<seque<int> > tet4sym{{0, 1, 2, 3}, {1, 2, 0, 3}, {2, 0, 1, 3}, {0, 3, 1, 2}, {3, 1, 0, 2}, {0, 2, 3, 1},
                                     {0, 3, 2, 1}, {3, 0, 1, 2}, {2, 1, 3, 0}, {1, 3, 0, 2}, {3, 2, 0, 1}, {2, 0, 3, 1}};
    const seque<seque<int> > wedge6{{0, 1, 2, 3, 4, 5}, {1, 2, 0, 4, 5, 3}, {2, 0, 1, 5, 3, 4},
                                    {0, 2, 1, 3, 5, 4}, {2, 1, 0, 5, 4, 3}, {1, 0, 2, 4, 3, 5}};
    zoo<MeshTypes> garden;
    garden.definesymmetrygroup<isanelement, isanelement>(emptysymm);
    garden.definesymmetrygroup<node, node>(emptysymm);
    garden.definesymmetrygroup<edge, node>(e2symm);
    garden.definesymmetrygroup<edge, isanelement>({{0}});
    garden.definesymmetrygroup<tri, node>(t3sym);
    garden.definesymmetrygroup<tri, isanelement>({{0}});
    garden.definesymmetrygroup<quad, node>(q4sym);
    garden.definesymmetrygroup<quad, isanelement>({{0}});
    garden.definesymmetrygroup<tet, node>(tet4sym);
    garden.definesymmetrygroup<tet, isanelement>({{0}});
    garden.definesymmetrygroup<wedge, node>(wedge6);
    garden.definesymmetrygroup<wedge, isanelement>({{0}});
    garden.definechildrenbuilder<tri, edge, node>({{0, 1}});
    garden.definechildrenbuilder<tri, edge, node>({{1, 2}});
    garden.definechildrenbuilder<tri, edge, node>({{2, 0}});
    garden.definechildrenbuilder<quad, edge, node>({{0, 1}});
    garden.definechildrenbuilder<quad, edge, node>({{1, 2}});
    garden.definechildrenbuilder<quad, edge, node>({{2, 3}});
    garden.definechildrenbuilder<quad, edge, node>({{3, 0}});
    garden.definechildrenbuilder<tet, tri, node>({{0, 1, 3}});
    garden.definechildrenbuilder<tet, tri, node>({{0, 3, 2}});
    garden.definechildrenbuilder<tet, tri, node>({{1, 0, 2}});
    garden.definechildrenbuilder<tet, tri, node>({{1, 2, 3}});
    garden.definechildrenbuilder<wedge, quad, node>({{1, 4, 5, 2}});
    garden.definechildrenbuilder<wedge, quad, node>({{1, 0, 3, 4}});
    garden.definechildrenbuilder<wedge, quad, node>({{0, 2, 5, 3}});
    garden.definechildrenbuilder<wedge, tri, node>({{0, 1, 2}});
    garden.definechildrenbuilder<wedge, tri, node>({{4, 3, 5}});
    // children are synthesized one level deep: edges of volumes are declared on the volume
    garden.definechildrenbuilder<tet, edge, node>({{0, 1}});
    garden.definechildrenbuilder<tet, edge, node>({{1, 2}});
    garden.definechildrenbuilder<tet, edge, node>({{2, 0}});
    garden.definechildrenbuilder<tet, edge, node>({{0, 3}});
    garden.definechildrenbuilder<tet, edge, node>({{1, 3}});
    garden.definechildrenbuilder<tet, edge, node>({{2, 3}});
    garden.definechildrenbuilder<wedge, edge, node>({{0, 1}});
    garden.definechildrenbuilder<wedge, edge, node>({{1, 2}});
    garden.definechildrenbuilder<wedge, edge, node>({{2, 0}});
    garden.definechildrenbuilder<wedge, edge, node>({{3, 4}});
    garden.definechildrenbuilder<wedge, edge, node>({{4, 5}});
    garden.definechildrenbuilder<wedge, edge, node>({{5, 3}});
    garden.definechildrenbuilder<wedge, edge, node>({{0, 3}});
    garden.definechildrenbuilder<wedge, edge, node>({{1, 4}});
    garden.definechildrenbuilder<wedge, edge, node>({{2, 5}});

    // the four generalized elements of Table 2(a): e0 tet {4,2,6,0}, e1 edge {3,0}, e2 edge {3,1}, e3 wedge {6,3,5,1,2,4}
    thing t0;
    garden.appendnodestoathing<tet, node>(t0, {4, 2, 6, 0});
    garden.appendnodestoathing<tet, isanelement>(t0, {0});
    garden.uploadelement(t0);
    thing e1;
    garden.appendnodestoathing<edge, node>(e1, {3, 0});
    garden.appendnodestoathing<edge, isanelement>(e1, {0});
    garden.uploadelement(e1);
    thing e2;
    garden.appendnodestoathing<edge, node>(e2, {3, 1});
    garden.appendnodestoathing<edge, isanelement>(e2, {0});
    garden.uploadelement(e2);
    thing w3;
    garden.appendnodestoathing<wedge, node>(w3, {6, 3, 5, 1, 2, 4});
    garden.appendnodestoathing<wedge, isanelement>(w3, {0});
    garden.uploadelement(w3);

    garden.compress();
    report(garden, "after insertion (before duplicate removal)");
    garden.deduplicate<edge, node>();
    garden.deduplicate<tri, node>();
    garden.deduplicate<quad, node>();
    report(garden, "after duplicate removal");

    // queries on the synthesized edge relation
    using TM = zoo<MeshTypes>::TypeManager;
    const int ntype = getnumber<node, TM>();
    const int etype = getnumber<edge, TM>();
    auto hits = getelementswithnodes(garden.synthesized, etype, ntype, {2, 4});
    std::cout << "synthesized edges containing {2,4}: " << getsize(hits) << "\n";
    auto hits2 = getelementsfromnodes(garden.synthesized, etype, ntype, {4, 2});
    std::cout << "synthesized edges defined by {4,2}: " << getsize(hits2) << "\n";

    // remove node 3 (member of e1, e2 and the wedge) and compress both matrices:
    // the wedge goes, and with it every child whose only parent it was
    garden.removenode<node>(3);
    garden.compress();
    report(garden, "after removal of node 3");
    // the same through an element: removing the tetrahedron e0 leaves nothing synthesized
    garden.removeelement<tet>(0);
    garden.compress();
    report(garden, "after removal of the tetrahedron e0");
    return 0;
}
