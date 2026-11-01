// Writes the node graph of the H8 mesh of nee^3 elements (the one bench_graph.cpp traverses) in the
// serialized format of the GAP benchmark suite (.sg: directed flag, edge count, node count, 64-bit
// offsets, 32-bit neighbors), so that GAP's bfs and cc run on exactly the same graph.
// Usage: dump_sg <nee> <file.sg>
#include "graph.hpp"
#include "m2m.hpp"
#include "o2m.hpp"
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <vector>

int main(int argc, char **argv)
{
    if (argc < 3)
    {
        std::fprintf(stderr, "usage: dump_sg <nee> <file.sg>\n");
        return 1;
    }
    const int nee = std::atoi(argv[1]);
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
    o2m ng = getnodegraph(mm);
    const long long nnz = getnnz(ng);
    std::FILE *f = std::fopen(argv[2], "wb");
    if (!f)
    {
        std::fprintf(stderr, "cannot open %s\n", argv[2]);
        return 2;
    }
    const bool directed = false;
    const std::int64_t num_edges = nnz, num_nodes = nn;
    std::fwrite(&directed, sizeof(bool), 1, f);
    std::fwrite(&num_edges, sizeof(std::int64_t), 1, f);
    std::fwrite(&num_nodes, sizeof(std::int64_t), 1, f);
    std::vector<std::int64_t> off((size_t) nn + 1);
    off[0] = 0;
    for (long i = 0; i < nn; ++i)
        off[(size_t) i + 1] = off[(size_t) i] + getsize(ng[(int) i]);
    std::fwrite(off.data(), sizeof(std::int64_t), off.size(), f);
    std::vector<int> buf;
    buf.reserve(1 << 22);
    for (long i = 0; i < nn; ++i)
    {
        crowview r = ng[(int) i];
        for (int k = 0; k < getsize(r); ++k)
            buf.push_back(r[k]);
        if (buf.size() >= (1u << 22) - 64)
        {
            std::fwrite(buf.data(), sizeof(int), buf.size(), f);
            buf.clear();
        }
    }
    if (!buf.empty())
        std::fwrite(buf.data(), sizeof(int), buf.size(), f);
    std::fclose(f);
    std::printf("{\"nee\": %d, \"nn\": %ld, \"nnz_graph_directed\": %lld, \"file\": \"%s\"}\n", nee, nn, nnz, argv[2]);
    return 0;
}
