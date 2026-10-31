// Performance tables of the manuscript (Sections 4.2-4.5) with the released library on
// structured H8 (nee^3 elements) and Q4 (nee^2 elements) meshes.
// Usage: bench_tables <group> <H8|Q4> <nee> [reps]
//   group = order     : lexicographic order, index maps of the unique rows, compression (gather + transpose)
//           ops       : A - B, A + B, A^T * A, A && B   (B = rows of A shifted by one element)
//           assembly  : Tr(A) and A^T * A (symbolic assembling)
//           cliques   : clique addressing of the element-node relation
//           search    : lookup by node list (library) and the build time of a std::map / std::unordered_map index
// One JSON line per call; every time is the median of reps runs.
#include "m2m.hpp"
#include "o2m.hpp"
#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <iostream>
#include <map>
#include <omp.h>
#include <random>
#include <string>
#include <unordered_map>
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

static o2m h8mesh(int nee, long &nn)
{
    const long nel = 1L * nee * nee * nee;
    nn = 1L * (nee + 1) * (nee + 1) * (nee + 1);
    o2m om;
    setsize(om, (int) nel);
    setsizeexact(om.data, 8 * nel);
    const int s1 = nee + 1, s2 = s1 * s1;
    int *d = om.data.begin();
    int *st = om.start.begin();
#pragma omp parallel for schedule(static) collapse(2)
    for (int iex = 0; iex < nee; ++iex)
        for (int iey = 0; iey < nee; ++iey)
            for (int iez = 0; iez < nee; ++iez)
            {
                const long e = (1L * iex * nee + iey) * nee + iez;
                const int base = iex + iey * s1 + iez * s2;
                int *p = d + 8 * e;
                p[0] = base; p[1] = base + 1; p[2] = base + 1 + s1; p[3] = base + s1;
                p[4] = base + s2; p[5] = base + 1 + s2; p[6] = base + 1 + s1 + s2; p[7] = base + s1 + s2;
                st[e] = (int) (8 * e);
            }
    st[nel] = (int) (8 * nel);
    om.maxnode = (int) nn - 1;
    return om;
}

static o2m q4mesh(int nee, long &nn)
{
    const long nel = 1L * nee * nee;
    nn = 1L * (nee + 1) * (nee + 1);
    o2m om;
    setsize(om, (int) nel);
    setsizeexact(om.data, 4 * nel);
    int *d = om.data.begin();
    int *st = om.start.begin();
#pragma omp parallel for schedule(static)
    for (int iex = 0; iex < nee; ++iex)
        for (int iey = 0; iey < nee; ++iey)
        {
            const long e = 1L * iex * nee + iey;
            int *p = d + 4 * e;
            p[0] = iex + iey * (nee + 1);
            p[1] = (iex + 1) + iey * (nee + 1);
            p[2] = (iex + 1) + (iey + 1) * (nee + 1);
            p[3] = iex + (iey + 1) * (nee + 1);
            st[e] = (int) (4 * e);
        }
    st[nel] = (int) (4 * nel);
    om.maxnode = (int) nn - 1;
    return om;
}

int main(int argc, char **argv)
{
    if (argc < 4)
    {
        std::cerr << "usage: bench_tables <order|ops|assembly|cliques|search> <H8|Q4> <nee> [reps]\n";
        return 2;
    }
    const std::string group = argv[1], kind = argv[2];
    const int nee = std::atoi(argv[3]);
    const int reps = argc > 4 ? std::atoi(argv[4]) : 5;
    long nn = 0;
    o2m om = kind == "H8" ? h8mesh(nee, nn) : q4mesh(nee, nn);
    const long nel = om.nelems();
    std::cout << "{\"group\": \"" << group << "\", \"kind\": \"" << kind << "\", \"threads\": " << omp_get_max_threads()
              << ", \"nee\": " << nee << ", \"nel\": " << nel << ", \"nn\": " << nn;
    if (group == "order")
    {
        seque<int> order, newfromold, oldfromnew;
        const double tord = timed(reps, [&] { order = getorder(om); });
        const double tidx = timed(reps, [&] { indicesfromorder(om, order, newfromold, oldfromnew); });
        m2m mm;
        mm.nfrome = om;
        const double tcmp = timed(reps, [&] {
            m2m copy;
            copy.nfrome = om;
            copy.isupdated = false;
            const double t0 = now();
            compresselements(copy, oldfromnew);
            return now() - t0;
        });
        // the copy above is outside the measured region: measure compression alone
        std::vector<double> tc;
        for (int r = 0; r < reps; ++r)
        {
            m2m copy;
            copy.nfrome = om;
            copy.isupdated = false;
            const double t0 = now();
            compresselements(copy, oldfromnew);
            tc.push_back(now() - t0);
        }
        (void) tcmp;
        std::cout << ", \"t_lexiorder\": " << tord << ", \"t_indicesfromorder\": " << tidx
                  << ", \"t_compression\": " << median(tc) << ", \"nunique\": " << getsize(oldfromnew);
    } else if (group == "ops")
    {
        // B: the rows of A shifted by one element (partial overlap of every row)
        seque<int> shift((int) nel);
        for (long e = 0; e < nel; ++e)
            shift[(int) e] = (int) ((e + 1) % nel);
        o2m bm = getrows(om, shift);
        o2m res, at;
        const double tdiff = timed(reps, [&] { res = om - bm; });
        const double tsum = timed(reps, [&] { res = om + bm; });
        const double tand = timed(reps, [&] { res = om && bm; });
        at = Tr(om);
        const double tmul = timed(reps, [&] { res = at * om; });
        std::cout << ", \"t_minus\": " << tdiff << ", \"t_plus\": " << tsum << ", \"t_times\": " << tmul
                  << ", \"t_and\": " << tand << ", \"nnz_times\": " << getnnz(res);
    } else if (group == "assembly")
    {
        o2m at, p;
        const double ttr = timed(reps, [&] { at = Tr(om); });
        const double tmul = timed(reps, [&] { p = at * om; });
        std::cout << ", \"t_transpose\": " << ttr << ", \"t_product\": " << tmul << ", \"t_assembling\": " << ttr + tmul
                  << ", \"nnz_product\": " << getnnz(p);
    } else if (group == "cliques")
    {
        m2m mm;
        mm.nfrome = om;
        mm.isupdated = false;
        synchronize(mm);
        o2m cl;
        const double tcl = timed(reps, [&] { cl = getcliques(mm); });
        std::cout << ", \"t_cliques\": " << tcl << ", \"nnz_cliques\": " << getnnz(cl);
    } else if (group == "search")
    {
        m2m mm;
        mm.nfrome = om;
        mm.isupdated = false;
        synchronize(mm);
        std::mt19937_64 rng(7);
        std::uniform_int_distribution<long> dist(0, nel - 1);
        const int nq = 100000;
        std::vector<int> q(nq);
        for (int i = 0; i < nq; ++i)
            q[i] = (int) dist(rng);
        int found = 0;
        const double tl = timed(reps, [&] {
            found = 0;
            for (int i = 0; i < nq; ++i)
            {
                seque<int> res = getelementswithnodes(mm, om[q[i]]);
                found += (getsize(res) == 1 && res[0] == q[i]);
            }
        }) / nq;
        // one-off index structures keyed by the sorted node list
        auto keyof = [&](int e) {
            seque<int> k = om[e];
            setordered(k);
            std::string s((const char *) k.begin(), (size_t) getsize(k) * sizeof(int));
            return s;
        };
        const double tumap = timed(std::min(reps, 3), [&] {
            std::unordered_map<std::string, int> idx;
            idx.reserve((size_t) nel);
            for (long e = 0; e < nel; ++e)
                idx.emplace(keyof((int) e), (int) e);
        });
        const double tmap = timed(std::min(reps, 3), [&] {
            std::map<std::string, int> idx;
            for (long e = 0; e < nel; ++e)
                idx.emplace(keyof((int) e), (int) e);
        });
        std::cout << ", \"t_lookup_us\": " << 1e6 * tl << ", \"found\": " << found << ", \"t_unordered_map_build\": "
                  << tumap << ", \"t_map_build\": " << tmap;
    }
    std::cout << "}" << std::endl;
    return 0;
}
