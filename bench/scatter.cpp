// Memory-system reference for the transpose: a parallel copy (bandwidth) and a random scatter of as
// many 4-byte integers as the element-node relation of the H8 mesh of nee^3 elements has incidences
// (8 nee^3), best of five. Usage: scatter [nee]   -> one JSON line
#include <algorithm>
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <omp.h>

int main(int argc, char **argv)
{
    const int nee = argc > 1 ? std::atoi(argv[1]) : 300;
    const std::size_t m = 8ull * nee * nee * nee;
    const std::size_t n = 128u << 20; // 128M doubles = 1 GB per array
    double *a = new double[n], *b = new double[n];
#pragma omp parallel for schedule(static)
    for (std::size_t i = 0; i < n; ++i)
    {
        a[i] = 1.0;
        b[i] = 2.0;
    }
    double best = 1e9;
    for (int r = 0; r < 5; ++r)
    {
        const auto t0 = std::chrono::steady_clock::now();
#pragma omp parallel for schedule(static)
        for (std::size_t i = 0; i < n; ++i)
            b[i] = a[i];
        best = std::min(best, std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count());
    }
    const double copy = best, gbs = 2.0 * n * 8 / best / 1e9;
    int *src = new int[m], *dst = new int[m];
    unsigned *idx = new unsigned[m];
#pragma omp parallel for schedule(static)
    for (std::size_t i = 0; i < m; ++i)
    {
        src[i] = (int) i;
        dst[i] = 0;
        idx[i] = (unsigned) ((i * 2654435761u) % m); // a permutation when m is odd, close to one otherwise
    }
    best = 1e9;
    for (int r = 0; r < 5; ++r)
    {
        const auto t0 = std::chrono::steady_clock::now();
#pragma omp parallel for schedule(static)
        for (std::size_t i = 0; i < m; ++i)
            dst[idx[i]] = src[i];
        best = std::min(best, std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count());
    }
    std::printf("{\"backend\": \"scatter\", \"threads\": %d, \"nee\": %d, \"n_scatter\": %zu, \"t_copy_1gb\": %.4f, \"copy_gbs\": %.1f, "
                "\"t_scatter\": %.4f, \"scatter_m_per_s\": %.0f, \"check\": %d}\n",
                omp_get_max_threads(), nee, m, copy, gbs, best, m / best / 1e6, dst[12345] != 0);
    return 0;
}
