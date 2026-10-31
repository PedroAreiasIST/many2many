// Library side of the same-machine comparison for the IJNME revision:
// symbolic transpose, node-to-node composition and element lookup by node list
// on a structured H8 mesh of nee^3 elements.  Usage: bench_lib <nee> [reps]
#include "m2m.hpp"
#include "o2m.hpp"
#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <iostream>
#include <omp.h>
#include <random>
#include <vector>

static double now()
{
    return std::chrono::duration<double>(std::chrono::steady_clock::now().time_since_epoch()).count();
}

int main(int argc, char **argv)
{
    const int nee = argc > 1 ? std::atoi(argv[1]) : 100;
    const int reps = argc > 2 ? std::atoi(argv[2]) : 3;
    const long nel = 1L * nee * nee * nee;
    const long nn = 1L * (nee + 1) * (nee + 1) * (nee + 1);
    o2m om;
    setsize(om, (int) nel);
    {
        const int s1 = nee + 1, s2 = s1 * s1;
        int e = 0;
        for (int iex = 0; iex < nee; ++iex)
            for (int iey = 0; iey < nee; ++iey)
                for (int iez = 0; iez < nee; ++iez)
                {
                    const int base = iex + iey * s1 + iez * s2;
                    seque<int> nodes(8);
                    nodes[0] = base;
                    nodes[1] = base + 1;
                    nodes[2] = base + 1 + s1;
                    nodes[3] = base + s1;
                    nodes[4] = base + s2;
                    nodes[5] = base + 1 + s2;
                    nodes[6] = base + 1 + s1 + s2;
                    nodes[7] = base + s1 + s2;
                    setrow(om, e++, nodes);
                }
        om.nelem = (int) nel;
        om.maxnode = (int) nn - 1;
    }
    std::vector<double> ttr, tmul;
    o2m at, p;
    for (int r = 0; r < reps; ++r)
    {
        double t0 = now();
        at = Tr(om);
        ttr.push_back(now() - t0);
    }
    for (int r = 0; r < reps; ++r)
    {
        double t0 = now();
        p = at * om;
        tmul.push_back(now() - t0);
    }
    long nnz = 0;
    for (int i = 0; i < p.nelems(); ++i) nnz += p.nnodes(i);
    std::sort(ttr.begin(), ttr.end());
    std::sort(tmul.begin(), tmul.end());
    p = o2m();
    // lookup through the bidirectional container
    m2m mm;
    mm.nfrome = om;
    mm.isupdated = false;
    synchronize(mm);
    std::mt19937_64 rng(1);
    std::uniform_int_distribution<long> dist(0, nel - 1);
    const int nq = 100000;
    std::vector<int> q(nq);
    for (int i = 0; i < nq; ++i) q[i] = (int) dist(rng);
    int found = 0;
    double t0 = now();
    for (int i = 0; i < nq; ++i)
    {
        seque<int> res = getelementswithnodes(mm, om[q[i]]);
        found += (getsize(res) == 1 && res[0] == q[i]);
    }
    double tl = (now() - t0) / nq;
    std::cout << "{\"backend\": \"library\", \"threads\": " << omp_get_max_threads() << ", \"nee\": " << nee
              << ", \"nel\": " << nel << ", \"nn\": " << nn << ", \"t_transpose\": " << ttr[reps / 2]
              << ", \"t_product\": " << tmul[reps / 2] << ", \"nnz_product\": " << nnz
              << ", \"t_lookup_us\": " << 1e6 * tl << ", \"found\": " << found << "}" << std::endl;
    return found == nq ? 0 : 1;
}
