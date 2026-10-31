// Library side of the derived-query comparison (SciPy side: bench_graph.py) on the H8 mesh of
// nee^3 elements: node-to-node graph, reverse Cuthill-McKee on a deterministically shuffled
// numbering, connected components (bipartite and on the node graph), breadth-first levels,
// plus the operations SciPy does not offer (face-neighbour dual graph, greedy colouring,
// k-hop neighbourhoods). Usage: bench_graph <nee> [reps]
#include "graph.hpp"
#include "m2m.hpp"
#include "o2m.hpp"
#include <algorithm>
#include <chrono>
#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <numeric>
#include <omp.h>
#include <vector>

static double now()
{
    return std::chrono::duration<double>(std::chrono::steady_clock::now().time_since_epoch()).count();
}

static double median(std::vector<double> v)
{
    std::sort(v.begin(), v.end());
    return v[v.size() / 2];
}

template<class F>
static double timed(int reps, F f)
{
    std::vector<double> t;
    for (int r = 0; r < reps; ++r)
    {
        const double t0 = now();
        f();
        t.push_back(now() - t0);
    }
    return median(t);
}

int main(int argc, char **argv)
{
    const int nee = argc > 1 ? std::atoi(argv[1]) : 100;
    const int reps = argc > 2 ? std::atoi(argv[2]) : 3;
    const long nel = 1L * nee * nee * nee;
    const long nn = 1L * (nee + 1) * (nee + 1) * (nee + 1);
    m2m mm;
    setsize(mm.nfrome, (int) nel);
    setsizeexact(mm.nfrome.data, 8 * nel);
    {
        const int s1 = nee + 1, s2 = s1 * s1;
        int *d = mm.nfrome.data.begin();
        int *st = mm.nfrome.start.begin();
#pragma omp parallel for schedule(static) collapse(2)
        for (int iex = 0; iex < nee; ++iex)
            for (int iey = 0; iey < nee; ++iey)
                for (int iez = 0; iez < nee; ++iez)
                {
                    const long e = (1L * iex * nee + iey) * nee + iez;
                    const int b = iex + iey * s1 + iez * s2;
                    int *p = d + 8 * e;
                    p[0] = b; p[1] = b + 1; p[2] = b + 1 + s1; p[3] = b + s1;
                    p[4] = b + s2; p[5] = b + 1 + s2; p[6] = b + 1 + s1 + s2; p[7] = b + s1 + s2;
                    st[e] = (int) (8 * e);
                }
        st[nel] = (int) (8 * nel);
        mm.nfrome.maxnode = (int) nn - 1;
    }
    mm.isupdated = false;
    synchronize(mm);
    std::cout << "{\"backend\": \"library\", \"threads\": " << omp_get_max_threads() << ", \"nee\": " << nee
              << ", \"nel\": " << nel << ", \"nn\": " << nn;
    // node graph (the product without the diagonal)
    o2m ng;
    const double tng = timed(reps, [&] { ng = getnodegraph(mm); });
    std::cout << ", \"nnz_graph\": " << getnnz(ng) + nn << ", \"t_nodegraph\": " << tng;
    // deterministic shuffle of the node numbering (same rule as bench_graph.py): order of the keys i*2654435761 mod 2^32
    seque<int> perm((int) nn);
    {
        std::vector<std::pair<std::uint32_t, int>> keys((size_t) nn);
        for (long i = 0; i < nn; ++i)
            keys[(size_t) i] = {static_cast<std::uint32_t>(static_cast<std::uint64_t>(i) * 2654435761ULL), (int) i};
        std::sort(keys.begin(), keys.end());
        for (long i = 0; i < nn; ++i)
            perm[keys[(size_t) i].second] = (int) i; // newnodefromold
    }
    m2m mp;
    mp.nfrome = mm.nfrome;
    hidden::permutenodes(mp.nfrome, perm);
    mp.isupdated = false;
    o2m ngp = getnodegraph(mp);
    const int bw0 = getbandwidth(ng), bwp = getbandwidth(ngp);
    seque<int> rcm;
    const double trcm = timed(reps, [&] { rcm = getrcmorder(ngp); });
    const int bwr = getbandwidth(ngp, rcm);
    std::cout << ", \"t_rcm\": " << trcm << ", \"bandwidth_natural\": " << bw0 << ", \"bandwidth_shuffled\": " << bwp
              << ", \"bandwidth_rcm\": " << bwr;
    // components
    seque<int> lab, el, nl;
    int ncomp = 0, ncel = 0;
    const double tcc = timed(reps, [&] { ncomp = getcomponents(ng, lab); });
    const double tccel = timed(reps, [&] { ncel = getcomponents(mm, el, nl); });
    std::cout << ", \"t_components_nodes\": " << tcc << ", \"ncomponents\": " << ncomp
              << ", \"t_components_elements\": " << tccel << ", \"ncomponents_elements\": " << ncel;
    // bfs levels from node 0
    seque<int> lev;
    const double tbfs = timed(reps, [&] { lev = getbfslevels(ng, 0); });
    std::cout << ", \"t_bfs\": " << tbfs << ", \"eccentricity\": " << *std::max_element(lev.begin(), lev.end());
    // face-neighbour dual graph, colouring, k-hop
    o2m dual;
    const double tdual = timed(reps, [&] { dual = getdualgraph(mm, 4); });
    seque<int> col;
    int ncol = 0;
    const double tcol = timed(reps, [&] { ncol = getcolouring(dual, col); });
    const int probe = (int) (nel / 2 + nee / 2);
    seque<int> hop;
    const double tk = timed(reps, [&] {
        for (int i = 0; i < 1000; ++i)
            hop = getkhopneighbourhood(mm, (probe + i) % (int) nel, 2, 4);
    }) / 1000;
    std::cout << ", \"t_dual_faces\": " << tdual << ", \"nnz_dual_faces\": " << getnnz(dual) << ", \"t_colouring\": " << tcol
              << ", \"ncolours\": " << ncol << ", \"t_khop2_us\": " << 1e6 * tk << ", \"khop2_size\": " << hop.size << "}"
              << std::endl;
    return 0;
}
