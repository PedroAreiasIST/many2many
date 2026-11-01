#include "graph.hpp"
#include <algorithm>
#include <atomic>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <numeric>
#include <stdexcept>
#include <vector>
#ifdef _OPENMP
#include <omp.h>
#endif

#define M2M_RESTRICT __restrict

namespace
{
    inline int numthreads()
    {
#ifdef _OPENMP
        return omp_get_max_threads();
#else
        return 1;
#endif
    }

    inline int threadnum()
    {
#ifdef _OPENMP
        return omp_get_thread_num();
#else
        return 0;
#endif
    }

    long long scan(const int *sizes, int n, int *start)
    {
        long long acc = 0;
        for (int i = 0; i < n; ++i)
        {
            start[i] = static_cast<int>(acc);
            acc += sizes[i];
        }
        start[n] = static_cast<int>(acc);
        if (acc > 2147483647LL)
            throw std::length_error("o2m: more than 2^31-1 entries");
        return acc;
    }

    // ------------------------------------------------------------------ counting composition
    // Row r of A, columns through B: multiplicity of every column c (counter per column,
    // cleared through the touched list); keeps c with multiplicity >= minshared, c != self.
    // The rows r0..r1 are written back to back at out (and their counts at oc); returns the
    // number of entries written. The kernel reads and writes pure CSR arrays that do not alias.
    template<class CNT>
    long long dualchunk(int r0, int r1, const int *M2M_RESTRICT as, const int *M2M_RESTRICT ad,
                        const int *M2M_RESTRICT bs, const int *M2M_RESTRICT bd, int minshared, bool excludeself,
                        CNT *M2M_RESTRICT cnt, int *M2M_RESTRICT rowsz, int *M2M_RESTRICT out, int *M2M_RESTRICT oc,
                        int *M2M_RESTRICT t)
    {
        long long pos = 0;
        for (int ra = r0; ra < r1; ++ra)
        {
            const int *pa = ad + as[ra];
            const int nka = as[ra + 1] - as[ra];
            int nt = 0;
            for (int ka = 0; ka < nka; ++ka)
            {
                const int ca = pa[ka];
                const int *pb = bd + bs[ca];
                const int nkb = bs[ca + 1] - bs[ca];
                for (int kb = 0; kb < nkb; ++kb)
                {
                    const int cb = pb[kb];
                    if (cnt[cb]++ == 0) // branchy: faster than the unconditional store, as the markers
                        t[nt++] = cb;
                }
            }
            int *o = out + pos;
            int *c2 = oc ? oc + pos : nullptr;
            int n = 0;
            for (int k = 0; k < nt; ++k)
            {
                const int c = t[k];
                if (cnt[c] >= minshared && !(excludeself && c == ra))
                {
                    o[n] = c;
                    if (c2)
                        c2[n] = static_cast<int>(cnt[c]);
                    ++n;
                }
                cnt[c] = 0;
            }
            rowsz[ra] = n;
            pos += n;
        }
        return pos;
    }

    /** Single pass, as the product: every chunk of 1024 rows is counted once into the buffer of
     *  the thread that took it (columns and, if asked, multiplicities) and copied to its final
     *  place after the scan of the row sizes. Peak memory: the bound of the output, twice. */
    template<class CNT>
    o2m countingcomposition(o2m const &a, o2m const &b, int minshared, bool excludeself, o2m *counts)
    {
        o2m res;
        const int na = a.nelem;
        const int ncols = b.maxnode + 1;
        res.maxnode = b.maxnode;
        setsize(res, na);
        if (na == 0)
        {
            if (counts)
                *counts = o2m();
            return res;
        }
        const int *as = a.start.begin(), *ad = a.data.begin(), *bs = b.start.begin(), *bd = b.data.begin();
        constexpr int CHUNK = 1024;
        const int nchunks = (na + CHUNK - 1) / CHUNK;
        const int nth = numthreads();
        struct chunkrec
        {
            int tid;
            long long off;
        };
        // staging budget, as the product (M2M_MUL_BUDGET_GB, 8 GB): beyond it the rows are
        // counted twice, the second time straight into the output
        long long budget = 8LL << 30;
        if (const char *env = std::getenv("M2M_MUL_BUDGET_GB"))
            budget = static_cast<long long>(std::atof(env) * (1LL << 30));
        long long totalbound = 0;
#pragma omp parallel for schedule(static) reduction(+ : totalbound) if (na > 100000)
        for (int ra = 0; ra < na; ++ra)
            for (int ka = as[ra]; ka < as[ra + 1]; ++ka)
                totalbound += bs[ad[ka] + 1] - bs[ad[ka]];
        const bool single = totalbound * (counts ? 8 : 4) <= budget;
        std::vector<chunkrec> rec(static_cast<std::size_t>(nchunks));
        std::vector<std::vector<int>> bufs(static_cast<std::size_t>(nth)), cbufs(counts ? static_cast<std::size_t>(nth) : 0);
        std::unique_ptr<int[]> rowsz(new int[static_cast<std::size_t>(na)]);
#pragma omp parallel num_threads(nth)
        {
            std::unique_ptr<CNT[]> cnt(new CNT[static_cast<std::size_t>(ncols) + 1]());
            const int tid = threadnum();
            std::vector<int> &buf = bufs[static_cast<std::size_t>(tid)];
            std::vector<int> *cbuf = counts ? &cbufs[static_cast<std::size_t>(tid)] : nullptr;
            buf.reserve(static_cast<std::size_t>(32) * (static_cast<std::size_t>(na) / static_cast<std::size_t>(nth) + CHUNK));
            if (cbuf)
                cbuf->reserve(buf.capacity());
            std::vector<int> scratch(1024);
#pragma omp for schedule(dynamic, 1)
            for (int c = 0; c < nchunks; ++c)
            {
                const int r0 = c * CHUNK, r1 = std::min(na, r0 + CHUNK);
                long long bound = 0;
                int rowbound = 0;
                for (int ra = r0; ra < r1; ++ra)
                {
                    const int *pa = ad + as[ra];
                    const int nka = as[ra + 1] - as[ra];
                    int rb = 0;
                    for (int ka = 0; ka < nka; ++ka)
                        rb += bs[pa[ka] + 1] - bs[pa[ka]];
                    bound += rb;
                    rowbound = std::max(rowbound, rb);
                }
                if (static_cast<std::size_t>(rowbound) > scratch.size())
                    scratch.resize(static_cast<std::size_t>(2 * rowbound));
                const long long off = single ? static_cast<long long>(buf.size()) : 0;
                buf.resize(static_cast<std::size_t>(off + bound));
                if (cbuf)
                    cbuf->resize(static_cast<std::size_t>(off + bound));
                const long long pos = dualchunk<CNT>(r0, r1, as, ad, bs, bd, minshared, excludeself, cnt.get(), rowsz.get(),
                                                     buf.data() + off, cbuf ? cbuf->data() + off : nullptr, scratch.data());
                if (single)
                {
                    buf.resize(static_cast<std::size_t>(off + pos));
                    if (cbuf)
                        cbuf->resize(static_cast<std::size_t>(off + pos));
                    rec[static_cast<std::size_t>(c)] = chunkrec{tid, off};
                }
            }
        }
        const long long nnz = scan(rowsz.get(), na, res.start.begin());
        setsizeexact(res.data, nnz);
        if (counts)
        {
            *counts = o2m();
            setsize(*counts, na);
            std::memcpy(counts->start.begin(), res.start.begin(), static_cast<size_t>(na + 1) * sizeof(int));
            setsizeexact(counts->data, nnz);
            counts->maxnode = 0;
        }
        const int *cs = res.start.begin();
        int *cd = res.data.begin();
        int *cc = counts ? counts->data.begin() : nullptr;
        if (single)
        {
#pragma omp parallel for schedule(dynamic, 1)
            for (int c = 0; c < nchunks; ++c)
            {
                const int r0 = c * CHUNK, r1 = std::min(na, r0 + CHUNK);
                const long long n = static_cast<long long>(cs[r1]) - cs[r0];
                if (n > 0)
                {
                    const chunkrec &r = rec[static_cast<std::size_t>(c)];
                    std::memcpy(cd + cs[r0], bufs[static_cast<std::size_t>(r.tid)].data() + r.off, static_cast<std::size_t>(n) * sizeof(int));
                    if (cc)
                        std::memcpy(cc + cs[r0], cbufs[static_cast<std::size_t>(r.tid)].data() + r.off, static_cast<std::size_t>(n) * sizeof(int));
                }
            }
        } else
        {
            bufs.clear();
            cbufs.clear();
#pragma omp parallel num_threads(nth)
            {
                std::unique_ptr<CNT[]> cnt(new CNT[static_cast<std::size_t>(ncols) + 1]());
                std::vector<int> scratch(1024);
#pragma omp for schedule(dynamic, 1)
                for (int c = 0; c < nchunks; ++c)
                {
                    const int r0 = c * CHUNK, r1 = std::min(na, r0 + CHUNK);
                    int rowbound = 0;
                    for (int ra = r0; ra < r1; ++ra)
                    {
                        int rb = 0;
                        for (int ka = as[ra]; ka < as[ra + 1]; ++ka)
                            rb += bs[ad[ka] + 1] - bs[ad[ka]];
                        rowbound = std::max(rowbound, rb);
                    }
                    if (static_cast<std::size_t>(rowbound) > scratch.size())
                        scratch.resize(static_cast<std::size_t>(2 * rowbound));
                    dualchunk<CNT>(r0, r1, as, ad, bs, bd, minshared, excludeself, cnt.get(), rowsz.get(), cd + cs[r0],
                                   cc ? cc + cs[r0] : nullptr, scratch.data());
                }
            }
        }
        if (counts)
        {
            int mx = 0;
#pragma omp parallel for schedule(static) reduction(max : mx)
            for (long long k = 0; k < nnz; ++k)
                mx = std::max(mx, cc[k]);
            counts->maxnode = mx;
        }
        return res;
    }

    /** Pack both relations (the kernels read pure CSR). */
    o2m packedcopy(o2m const &rel)
    {
        if (!rel.isjagged())
            return rel;
        o2m copy = rel;
        pack(copy);
        return copy;
    }

} // namespace

o2m getdualgraph(m2m &rel, int minshared, o2m *counts)
{
    synchronize(rel);
    o2m a = packedcopy(rel.nfrome), b = packedcopy(rel.efromn);
    if (minshared < 1)
        minshared = 1;
    // the multiplicity is at most the arity of an element; one byte suffices below 256
    int maxarity = 0;
    for (int e = 0; e < a.nelem; ++e)
        maxarity = std::max(maxarity, a.nnodes(e));
    if (maxarity < 255)
        return countingcomposition<unsigned char>(a, b, minshared, true, counts);
    return countingcomposition<int>(a, b, minshared, true, counts);
}

o2m getnodegraph(m2m &rel)
{
    synchronize(rel);
    o2m a = packedcopy(rel.efromn), b = packedcopy(rel.nfrome);
    int maxvalence = 0;
    for (int n = 0; n < a.nelem; ++n)
        maxvalence = std::max(maxvalence, a.nnodes(n));
    if (maxvalence < 255)
        return countingcomposition<unsigned char>(a, b, 1, true, nullptr);
    return countingcomposition<int>(a, b, 1, true, nullptr);
}

seque<int> getelementssharing(m2m &rel, int element, int minshared, seque<int> *counts)
{
    synchronize(rel);
    // candidates with multiplicities through a small sorted merge: gather, sort, run-length count
    seque<int> cand;
    for (int node: rel.nfrome[element])
        for (int e: rel.efromn[node])
            if (e != element)
                append(cand, e);
    setordered(cand);
    seque<int> res;
    if (counts)
        setsize(*counts, 0);
    for (int i = 0; i < cand.size;)
    {
        int j = i;
        while (j < cand.size && cand[j] == cand[i])
            ++j;
        if (j - i >= minshared)
        {
            append(res, cand[i]);
            if (counts)
                append(*counts, j - i);
        }
        i = j;
    }
    return res;
}

seque<int> getkhopneighborhood(m2m &rel, int element, int k, int minshared)
{
    synchronize(rel);
    seque<int> visited{element}, frontier{element};
    std::vector<unsigned char> seen;
    // a small open hash would do; the neighborhoods are tiny, so sorted sequences suffice
    for (int hop = 0; hop < k && frontier.size > 0; ++hop)
    {
        seque<int> next;
        for (int i = 0; i < frontier.size; ++i)
        {
            seque<int> nb = getelementssharing(rel, frontier[i], minshared);
            for (int j = 0; j < nb.size; ++j)
                append(next, nb[j]);
        }
        setorderedandunique(next);
        next = getdifference(next, visited);
        visited = getunion(visited, next);
        frontier = next;
    }
    return visited;
}

namespace
{
    // ------------------------------------------------------------------ components by subgraph sampling
    /** Connected components in the manner of Afforest (Sutton, Ben-Nun and Barak, IPDPS 2018): every
     *  vertex first hooks to its first two neighbors only, which on a mesh already joins almost
     *  everything into one component; that component is identified from a sample, and only the
     *  vertices outside it visit their remaining neighbors. Hooking is by compare-and-swap of the
     *  larger root under the smaller, so the root of every component ends as its smallest vertex
     *  whichever thread wins, and the labels are deterministic. The neighbor access is a pair of
     *  callables (degree, k-th neighbor) so that the bipartite element-node graph needs no copy. */
    inline void hook(int u, int v, std::atomic<int> *M2M_RESTRICT comp) noexcept
    {
        int p1 = comp[u].load(std::memory_order_relaxed), p2 = comp[v].load(std::memory_order_relaxed);
        while (p1 != p2)
        {
            const int high = std::max(p1, p2), low = std::min(p1, p2);
            const int phigh = comp[high].load(std::memory_order_relaxed);
            if (phigh == low)
                break;
            int expected = high;
            if (phigh == high && comp[high].compare_exchange_strong(expected, low, std::memory_order_relaxed))
                break;
            p1 = comp[comp[high].load(std::memory_order_relaxed)].load(std::memory_order_relaxed);
            p2 = comp[low].load(std::memory_order_relaxed);
        }
    }

    inline void shortcut(int n, std::atomic<int> *M2M_RESTRICT comp) noexcept
    {
#pragma omp parallel for schedule(dynamic, 16384)
        for (int v = 0; v < n; ++v)
        {
            int p = comp[v].load(std::memory_order_relaxed);
            int gp = comp[p].load(std::memory_order_relaxed);
            while (p != gp)
            {
                comp[v].store(gp, std::memory_order_relaxed);
                p = gp;
                gp = comp[p].load(std::memory_order_relaxed);
            }
        }
    }

    /** The most frequent root among 1024 vertices drawn with a fixed linear congruential sequence. */
    inline int frequentroot(int n, std::atomic<int> const *M2M_RESTRICT comp) noexcept
    {
        std::vector<std::pair<int, int>> s;
        s.reserve(1024);
        std::uint64_t x = 88172645463325252ULL;
        for (int i = 0; i < 1024; ++i)
        {
            x = x * 6364136223846793005ULL + 1442695040888963407ULL;
            s.emplace_back(comp[static_cast<int>((x >> 33) % static_cast<std::uint64_t>(n))].load(std::memory_order_relaxed), 0);
        }
        std::sort(s.begin(), s.end());
        int best = s[0].first, bestcount = 0;
        for (std::size_t i = 0; i < s.size();)
        {
            std::size_t j = i;
            while (j < s.size() && s[j].first == s[i].first)
                ++j;
            if (static_cast<int>(j - i) > bestcount)
            {
                bestcount = static_cast<int>(j - i);
                best = s[i].first;
            }
            i = j;
        }
        return best;
    }

    template<class Degree, class Neighbor>
    std::unique_ptr<std::atomic<int>[]> afforest(int n, Degree degree, Neighbor neighbor)
    {
        std::unique_ptr<std::atomic<int>[]> comp(new std::atomic<int>[static_cast<std::size_t>(std::max(n, 1))]);
#pragma omp parallel for schedule(static)
        for (int v = 0; v < n; ++v)
            comp[v].store(v, std::memory_order_relaxed);
        if (n == 0)
            return comp;
        const int rounds = 2;
        for (int r = 0; r < rounds; ++r)
        {
#pragma omp parallel for schedule(dynamic, 16384)
            for (int v = 0; v < n; ++v)
                if (r < degree(v))
                    hook(v, neighbor(v, r), comp.get());
            shortcut(n, comp.get());
        }
        const int c = frequentroot(n, comp.get());
#pragma omp parallel for schedule(dynamic, 16384)
        for (int v = 0; v < n; ++v)
        {
            if (comp[v].load(std::memory_order_relaxed) == c)
                continue;
            const int d = degree(v);
            for (int k = rounds; k < d; ++k)
                hook(v, neighbor(v, k), comp.get());
        }
        shortcut(n, comp.get());
        return comp;
    }

    /** Component ids in order of the smallest member, from a root array whose roots are minima. */
    int compactlabels(int n, std::atomic<int> const *M2M_RESTRICT comp, seque<int> &label)
    {
        setsize(label, n);
        int *M2M_RESTRICT lab = label.begin();
        // roots get provisional negative ids in index order: counted per chunk, then numbered in parallel
        const int chunk = 1 << 16;
        const int nchunks = (n + chunk - 1) / chunk;
        std::vector<int> first(static_cast<std::size_t>(nchunks) + 1, 0);
#pragma omp parallel for schedule(static)
        for (int c = 0; c < nchunks; ++c)
        {
            int cnt = 0;
            const int e = std::min(n, (c + 1) * chunk);
            for (int i = c * chunk; i < e; ++i)
            {
                lab[i] = comp[i].load(std::memory_order_relaxed);
                cnt += lab[i] == i;
            }
            first[static_cast<std::size_t>(c) + 1] = cnt;
        }
        for (int c = 0; c < nchunks; ++c)
            first[static_cast<std::size_t>(c) + 1] += first[static_cast<std::size_t>(c)];
        const int ncomp = first[static_cast<std::size_t>(nchunks)];
#pragma omp parallel for schedule(static)
        for (int c = 0; c < nchunks; ++c)
        {
            int id = first[static_cast<std::size_t>(c)];
            const int e = std::min(n, (c + 1) * chunk);
            for (int i = c * chunk; i < e; ++i)
                if (lab[i] == i)
                    lab[i] = -(++id);
        }
        // members read their root's id (roots stay negative in this pass, so the reads race with nothing)
#pragma omp parallel for schedule(static)
        for (int i = 0; i < n; ++i)
            if (lab[i] >= 0)
                lab[i] = -lab[lab[i]] - 1;
#pragma omp parallel for schedule(static)
        for (int i = 0; i < n; ++i)
            if (lab[i] < 0)
                lab[i] = -lab[i] - 1;
        return ncomp;
    }
} // namespace

int getcomponents(o2m const &graph, seque<int> &label)
{
    const int n = graph.nelem;
    const int *M2M_RESTRICT gs = graph.start.begin();
    const int *M2M_RESTRICT gd = graph.data.begin();
    if (graph.size.size > 0)
    {
        const int *M2M_RESTRICT gz = graph.size.begin();
        auto comp = afforest(n, [gz](int v) { return gz[v]; }, [gs, gd](int v, int k) { return gd[gs[v] + k]; });
        return compactlabels(n, comp.get(), label);
    }
    auto comp = afforest(n, [gs](int v) { return gs[v + 1] - gs[v]; }, [gs, gd](int v, int k) { return gd[gs[v] + k]; });
    return compactlabels(n, comp.get(), label);
}

int getcomponents(m2m &rel, seque<int> &elementlabel, seque<int> &nodelabel)
{
    synchronize(rel);
    const int nel = rel.nfrome.nelem;
    const int nn = rel.nfrome.maxnode + 1;
    o2m const &a = rel.nfrome, &t = rel.efromn;
    // bipartite graph: vertices 0..nel-1 are the elements, nel..nel+nn-1 the nodes
    const int *M2M_RESTRICT as = a.start.begin(), *M2M_RESTRICT ad = a.data.begin();
    const int *M2M_RESTRICT ts = t.start.begin(), *M2M_RESTRICT td = t.data.begin();
    const int *M2M_RESTRICT az = a.size.size > 0 ? a.size.begin() : nullptr;
    const int *M2M_RESTRICT tz = t.size.size > 0 ? t.size.begin() : nullptr;
    auto degree = [=](int v) {
        return v < nel ? (az ? az[v] : as[v + 1] - as[v]) : (tz ? tz[v - nel] : ts[v - nel + 1] - ts[v - nel]);
    };
    auto neighbor = [=](int v, int k) { return v < nel ? nel + ad[as[v] + k] : td[ts[v - nel] + k]; };
    auto comp = afforest(nel + nn, degree, neighbor);
    seque<int> all;
    const int ncomp = compactlabels(nel + nn, comp.get(), all);
    setsize(elementlabel, nel);
    setsize(nodelabel, nn);
    int *M2M_RESTRICT el = elementlabel.begin(), *M2M_RESTRICT nl = nodelabel.begin();
    const int *M2M_RESTRICT al = all.begin();
#pragma omp parallel for schedule(static)
    for (int e = 0; e < nel; ++e)
        el[e] = al[e];
#pragma omp parallel for schedule(static)
    for (int v = 0; v < nn; ++v)
        nl[v] = al[nel + v];
    return ncomp;
}

namespace
{
    /** Level-synchronous breadth-first search from s over a symmetric adjacency: lv must hold -1
     *  everywhere; on return lv[v] is the hop distance of every reached vertex, last holds the
     *  vertices of the deepest level (in no particular order) and the depth is returned. The
     *  frontier is expanded in parallel, a vertex being claimed by the first thread that swaps
     *  its -1 for the level, so the levels are the same whichever thread wins. */
    int bfs(o2m const &g, int s, int *M2M_RESTRICT lv, std::vector<int> &last)
    {
        const int *M2M_RESTRICT gs = g.start.begin();
        const int *M2M_RESTRICT gd = g.data.begin();
        std::vector<int> frontier{s}, next;
        lv[s] = 0;
        const int nth = numthreads();
        std::vector<std::vector<int>> local(static_cast<std::size_t>(nth));
        std::vector<std::size_t> offs(static_cast<std::size_t>(nth) + 1);
        int d = 0;
        for (;;)
        {
            const int nf = static_cast<int>(frontier.size());
            const int dn = d + 1;
            next.clear();
            if (nf < 4096 || nth == 1)
            {
                for (int i = 0; i < nf; ++i)
                {
                    const int u = frontier[static_cast<std::size_t>(i)];
                    for (int k = gs[u]; k < gs[u + 1]; ++k)
                    {
                        const int v = gd[k];
                        if (lv[v] < 0)
                        {
                            lv[v] = dn;
                            next.push_back(v);
                        }
                    }
                }
            } else
            {
#pragma omp parallel num_threads(nth)
                {
                    const int tid = threadnum();
                    std::vector<int> &out = local[static_cast<std::size_t>(tid)];
                    out.clear();
#pragma omp for schedule(dynamic, 256) nowait
                    for (int i = 0; i < nf; ++i)
                    {
                        const int u = frontier[static_cast<std::size_t>(i)];
                        for (int k = gs[u]; k < gs[u + 1]; ++k)
                        {
                            const int v = gd[k];
                            if (__atomic_load_n(lv + v, __ATOMIC_RELAXED) < 0)
                            {
                                int expected = -1;
                                if (__atomic_compare_exchange_n(lv + v, &expected, dn, false, __ATOMIC_RELAXED, __ATOMIC_RELAXED))
                                    out.push_back(v);
                            }
                        }
                    }
                    offs[static_cast<std::size_t>(tid) + 1] = out.size();
#pragma omp barrier
#pragma omp single
                    {
                        offs[0] = 0;
                        for (int t = 0; t < nth; ++t)
                            offs[static_cast<std::size_t>(t) + 1] += offs[static_cast<std::size_t>(t)];
                        next.resize(offs[static_cast<std::size_t>(nth)]);
                    }
                    if (!out.empty())
                        std::memcpy(next.data() + offs[static_cast<std::size_t>(tid)], out.data(), out.size() * sizeof(int));
                }
            }
            if (next.empty())
                break;
            ++d;
            frontier.swap(next);
        }
        last.swap(frontier);
        return d;
    }

    void fillminusone(int *p, int n)
    {
#pragma omp parallel for schedule(static) if (n > 100000)
        for (int i = 0; i < n; ++i)
            p[i] = -1;
    }

    /** George-Liu pseudo-peripheral vertex of the component containing s: from the deepest
     *  level of the search from s, the vertex of least degree (least index among equals)
     *  restarts the search while the eccentricity grows. */
    int pseudoperipheral(o2m const &g, int s, int *lv, std::vector<int> &last)
    {
        fillminusone(lv, g.nelem);
        int depth = bfs(g, s, lv, last);
        for (int iter = 0; iter < 10; ++iter)
        {
            int best = last[0];
            for (int u: last)
            {
                const int du = g.nnodes(u), db = g.nnodes(best);
                if (du < db || (du == db && u < best))
                    best = u;
            }
            fillminusone(lv, g.nelem);
            const int d2 = bfs(g, best, lv, last);
            if (d2 <= depth)
                return best;
            depth = d2;
            s = best;
        }
        return s;
    }
} // namespace

seque<int> getbfslevels(o2m const &graph, int source)
{
    const int n = graph.nelem;
    seque<int> level(n);
    fillminusone(level.begin(), n);
    if (n == 0 || source < 0 || source >= n)
        return level;
    std::vector<int> last;
    bfs(graph, source, level.begin(), last);
    return level;
}

seque<int> getrcmorder(o2m const &graph)
{
    const int n = graph.nelem;
    seque<int> oldfromnew(n);
    if (n == 0)
        return oldfromnew;
    const int *M2M_RESTRICT gs = graph.start.begin();
    const int *M2M_RESTRICT gd = graph.data.begin();
    std::vector<unsigned char> visited(static_cast<std::size_t>(n), 0);
    std::vector<int> level(static_cast<std::size_t>(n)), last;
    std::vector<std::uint64_t> nb;
    int *M2M_RESTRICT q = oldfromnew.begin();
    int pos = 0;
    for (int s0 = 0; s0 < n; ++s0)
    {
        if (visited[static_cast<std::size_t>(s0)])
            continue;
        const int s = pseudoperipheral(graph, s0, level.data(), last);
        // Cuthill-McKee from s: the unvisited neighbors of every dequeued vertex are appended
        // in increasing degree (ties by index). The queue is known ahead of the head, so the
        // rows of the vertices that follow are prefetched while the current one is sorted.
        const int first = pos;
        q[pos++] = s;
        visited[static_cast<std::size_t>(s)] = 1;
        for (int head = first; head < pos; ++head)
        {
            if (head + 16 < pos)
                __builtin_prefetch(gs + q[head + 16]);
            if (head + 8 < pos)
                __builtin_prefetch(gd + gs[q[head + 8]]);
            const int u = q[head];
            nb.clear();
            for (int k = gs[u]; k < gs[u + 1]; ++k)
            {
                const int v = gd[k];
                if (!visited[static_cast<std::size_t>(v)])
                {
                    visited[static_cast<std::size_t>(v)] = 1;
                    nb.push_back((static_cast<std::uint64_t>(gs[v + 1] - gs[v]) << 32) | static_cast<std::uint32_t>(v));
                }
            }
            // insertion sort of the (degree, index) keys: the rows are short
            const int m = static_cast<int>(nb.size());
            std::uint64_t *p = nb.data();
            if (m < 32)
            {
                for (int i = 1; i < m; ++i)
                {
                    const std::uint64_t x = p[i];
                    int j = i - 1;
                    while (j >= 0 && p[j] > x)
                    {
                        p[j + 1] = p[j];
                        --j;
                    }
                    p[j + 1] = x;
                }
            } else
                std::sort(p, p + m);
            for (int i = 0; i < m; ++i)
                q[pos++] = static_cast<int>(p[i] & 0xffffffffu);
        }
        // reverse the component's segment
        std::reverse(q + first, q + pos);
    }
    return oldfromnew;
}

int getbandwidth(o2m const &graph, seque<int> const &oldfromnew)
{
    const int n = graph.nelem;
    seque<int> newfromold(n);
    if (oldfromnew.size == n)
        for (int i = 0; i < n; ++i)
            newfromold[oldfromnew[i]] = i;
    else
        std::iota(newfromold.begin(), newfromold.end(), 0);
    int bw = 0;
#pragma omp parallel for schedule(static) reduction(max : bw)
    for (int u = 0; u < n; ++u)
    {
        const int iu = newfromold[u];
        for (int v: graph[u])
            bw = std::max(bw, std::abs(iu - newfromold[v]));
    }
    return bw;
}

int getcoloring(o2m const &graph, seque<int> &color)
{
    const int n = graph.nelem;
    setsize(color, n);
    for (int i = 0; i < n; ++i)
        color[i] = -1;
    std::vector<int> used;
    int ncolors = 0;
    for (int u = 0; u < n; ++u)
    {
        used.assign(static_cast<std::size_t>(ncolors) + 1, 0);
        for (int v: graph[u])
            if (color[v] >= 0)
                used[static_cast<std::size_t>(color[v])] = 1;
        int c = 0;
        while (used[static_cast<std::size_t>(c)])
            ++c;
        color[u] = c;
        ncolors = std::max(ncolors, c + 1);
    }
    return ncolors;
}

int appendelementsynchronized(m2m &rel, crowview nodes)
{
    synchronize(rel);
    const int e = appendelement(rel.nfrome, nodes);
    o2m &t = rel.efromn;
    const int ncols = rel.nfrome.maxnode + 1;
    if (t.nelem < ncols)
        setsize(t, ncols);
    for (int i = 0; i < nodes.n; ++i)
        appendtorow(t, nodes.p[i], e);
    t.maxnode = e;
    rel.isupdated = true;
    rel.islocated = false;
    return e;
}
