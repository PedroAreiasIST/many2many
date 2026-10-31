// Checks of the derived queries (graph.hpp) against brute force on a small H8 grid and on a
// randomly renumbered copy of it.
#include "graph.hpp"
#include "m2m.hpp"
#include <algorithm>
#include <cstdio>
#include <iostream>
#include <map>
#include <numeric>
#include <random>
#include <set>
#include <vector>

static o2m h8mesh(int nee, int &nn)
{
    const int nel = nee * nee * nee;
    nn = (nee + 1) * (nee + 1) * (nee + 1);
    o2m om;
    setsize(om, nel);
    const int s1 = nee + 1, s2 = s1 * s1;
    int e = 0;
    for (int iex = 0; iex < nee; ++iex)
        for (int iey = 0; iey < nee; ++iey)
            for (int iez = 0; iez < nee; ++iez)
            {
                const int b = iex + iey * s1 + iez * s2;
                int nodes[8] = {b, b + 1, b + 1 + s1, b + s1, b + s2, b + 1 + s2, b + 1 + s1 + s2, b + s1 + s2};
                setrow(om, e++, crowview(nodes, 8));
            }
    om.maxnode = nn - 1;
    return om;
}

static int fails = 0;
#define CHECK(c, msg) do { if (!(c)) { ++fails; std::printf("FAIL: %s\n", msg); } } while (0)

int main()
{
    int nn = 0;
    m2m mm;
    mm.nfrome = h8mesh(6, nn);
    mm.isupdated = false;
    synchronize(mm);
    const int nel = mm.nfrome.nelems();

    // dual graph with shared counts vs brute force
    o2m counts;
    o2m dual = getdualgraph(mm, 1, &counts);
    o2m faces = getdualgraph(mm, 4);
    int nface = 0;
    for (int e = 0; e < nel; ++e)
    {
        std::map<int, int> brute;
        for (int a: mm.nfrome[e])
            for (int f: mm.efromn[a])
                if (f != e)
                    brute[f]++;
        CHECK(dual.nnodes(e) == (int) brute.size(), "dual graph row size");
        for (int k = 0; k < dual.nnodes(e); ++k)
            CHECK(brute[dual[e][k]] == counts[e][k], "dual graph shared count");
        int nf = 0;
        for (auto &kv: brute)
            nf += kv.second >= 4;
        CHECK(faces.nnodes(e) == nf, "face-neighbour count");
        nface += nf;
        seque<int> sc, sh = getelementssharing(mm, e, 4, &sc);
        CHECK(sh.size == nf, "getelementssharing count");
    }
    CHECK(nface == 2 * 3 * 6 * 6 * 5, "total face adjacencies of a 6x6x6 grid");

    // k-hop: hop 1 through faces = 1 + face neighbours; interior element has 6
    {
        seque<int> h1 = getkhopneighbourhood(mm, 6 * 6 * 2 + 6 * 2 + 2, 1, 4);
        CHECK(h1.size == 7, "1-hop face neighbourhood of an interior element");
        seque<int> h2 = getkhopneighbourhood(mm, 6 * 6 * 2 + 6 * 2 + 2, 2, 4);
        CHECK(h2.size == 25, "2-hop face neighbourhood (octahedral ball of radius 2)");
    }

    // components: one, then two after dropping a slab of elements
    {
        seque<int> lab;
        CHECK(getcomponents(dual, lab) == 1, "one component (dual)");
        seque<int> el, nl;
        CHECK(getcomponents(mm, el, nl) == 1, "one component (bipartite)");
        o2m cut = mm.nfrome;
        seque<int> keep;
        for (int e = 0; e < nel; ++e)
            if (e / 36 != 3)
                append(keep, e); // remove the slab iex == 3
        hidden::compresselements(cut, keep);
        m2m m2;
        m2.nfrome = cut;
        m2.isupdated = false;
        o2m d2 = getdualgraph(m2, 1);
        CHECK(getcomponents(d2, lab) == 2, "two components after removing a slab (dual)");
        CHECK(getcomponents(m2, el, nl) == 2 + 0, "two components (bipartite)");
        // nodes of the removed slab's interior faces belong to the two components; labels are 0/1
        CHECK(*std::max_element(el.begin(), el.end()) == 1, "labels compact");
    }

    // node graph, RCM and bandwidth on a randomly renumbered mesh
    {
        o2m ng = getnodegraph(mm);
        const int bw0 = getbandwidth(ng);
        std::mt19937 rng(3);
        seque<int> perm(nn);
        std::iota(perm.begin(), perm.end(), 0);
        std::shuffle(perm.begin(), perm.end(), rng);
        m2m mp;
        mp.nfrome = mm.nfrome;
        hidden::permutenodes(mp.nfrome, perm);
        mp.isupdated = false;
        o2m ngp = getnodegraph(mp);
        const int bwp = getbandwidth(ngp);
        seque<int> rcm = getrcmorder(ngp);
        seque<int> sorted = rcm;
        setordered(sorted);
        bool isperm = sorted.size == nn;
        for (int i = 0; i < sorted.size && isperm; ++i)
            isperm = sorted[i] == i;
        CHECK(isperm, "rcm is a permutation");
        const int bwr = getbandwidth(ngp, rcm);
        std::printf("bandwidth natural %d, shuffled %d, after RCM %d\n", bw0, bwp, bwr);
        // the lexicographic numbering of a cube is already a narrow band; RCM recovers a band
        // of the order of the largest diagonal level set, about twice that of the natural one
        CHECK(bwr < bwp / 2, "RCM reduces the bandwidth of the shuffled mesh");
        CHECK(bwr <= 3 * bw0, "RCM bandwidth of the order of the natural one");
        seque<int> lev = getbfslevels(ng, 0);
        CHECK(lev[nn - 1] == 6, "bfs level of the far corner node (27-point stencil: Chebyshev distance)");
    }

    // colouring: no two adjacent elements share a colour
    {
        seque<int> col;
        const int nc = getcolouring(dual, col);
        bool ok = true;
        for (int e = 0; e < nel && ok; ++e)
            for (int f: dual[e])
                if (col[e] == col[f])
                    ok = false;
        CHECK(ok, "proper colouring");
        std::printf("colours used: %d\n", nc);
        CHECK(nc <= 27, "at most 27 colours for an H8 grid");
    }

    // synchronized append equals a rebuild
    {
        m2m a;
        a.nfrome = h8mesh(3, nn);
        a.isupdated = false;
        synchronize(a);
        int nodes[8] = {0, 1, 5, 4, 16, 17, 21, 20};
        appendelementsynchronized(a, crowview(nodes, 8));
        m2m b;
        b.nfrome = a.nfrome;
        b.isupdated = false;
        synchronize(b);
        bool same = a.efromn.nelems() == b.efromn.nelems();
        for (int v = 0; v < b.efromn.nelems() && same; ++v)
        {
            seque<int> ra = a.efromn[v], rb = b.efromn[v];
            setordered(ra);
            same = ra == rb;
        }
        CHECK(same, "appendelementsynchronized keeps the transpose equal to a rebuild");
        CHECK(a.efromn.isjagged(), "transpose jagged after synchronized append");
        pack(a.efromn);
        CHECK(!a.efromn.isjagged(), "pack restores the packed state");
    }
    std::printf(fails ? "%d FAILURES\n" : "all graph checks passed\n", fails);
    return fails != 0;
}
