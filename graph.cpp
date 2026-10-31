#include "graph.hpp"
#include <algorithm>
#include <atomic>
#include <cstdint>
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
    template<class CNT>
    void dualcount(int r0, int r1, const int *M2M_RESTRICT as, const int *M2M_RESTRICT ad,
                   const int *M2M_RESTRICT bs, const int *M2M_RESTRICT bd, int minshared, bool excludeself,
                   CNT *M2M_RESTRICT cnt, int *M2M_RESTRICT rowsz, std::vector<int> &scratch)
    {
        for (int ra = r0; ra < r1; ++ra)
        {
            const int *pa = ad + as[ra];
            const int nka = as[ra + 1] - as[ra];
            int bound = 0;
            for (int ka = 0; ka < nka; ++ka)
                bound += bs[pa[ka] + 1] - bs[pa[ka]];
            if (static_cast<std::size_t>(bound) > scratch.size())
                scratch.resize(static_cast<std::size_t>(2 * bound));
            int *M2M_RESTRICT t = scratch.data();
            int nt = 0;
            for (int ka = 0; ka < nka; ++ka)
            {
                const int ca = pa[ka];
                const int *pb = bd + bs[ca];
                const int nkb = bs[ca + 1] - bs[ca];
                for (int kb = 0; kb < nkb; ++kb)
                {
                    const int cb = pb[kb];
                    t[nt] = cb;
                    nt += (cnt[cb] == 0);
                    cnt[cb]++;
                }
            }
            int n = 0;
            for (int k = 0; k < nt; ++k)
            {
                const int c = t[k];
                n += (cnt[c] >= minshared) && !(excludeself && c == ra);
                cnt[c] = 0;
            }
            rowsz[ra] = n;
        }
    }

    template<class CNT>
    void dualfill(int r0, int r1, const int *M2M_RESTRICT as, const int *M2M_RESTRICT ad,
                  const int *M2M_RESTRICT bs, const int *M2M_RESTRICT bd, int minshared, bool excludeself,
                  CNT *M2M_RESTRICT cnt, const int *M2M_RESTRICT cs, int *M2M_RESTRICT cd, int *M2M_RESTRICT cc,
                  std::vector<int> &scratch)
    {
        for (int ra = r0; ra < r1; ++ra)
        {
            const int *pa = ad + as[ra];
            const int nka = as[ra + 1] - as[ra];
            int bound = 0;
            for (int ka = 0; ka < nka; ++ka)
                bound += bs[pa[ka] + 1] - bs[pa[ka]];
            if (static_cast<std::size_t>(bound) > scratch.size())
                scratch.resize(static_cast<std::size_t>(2 * bound));
            int *M2M_RESTRICT t = scratch.data();
            int nt = 0;
            for (int ka = 0; ka < nka; ++ka)
            {
                const int ca = pa[ka];
                const int *pb = bd + bs[ca];
                const int nkb = bs[ca + 1] - bs[ca];
                for (int kb = 0; kb < nkb; ++kb)
                {
                    const int cb = pb[kb];
                    t[nt] = cb;
                    nt += (cnt[cb] == 0);
                    cnt[cb]++;
                }
            }
            int *out = cd + cs[ra];
            int *oc = cc ? cc + cs[ra] : nullptr;
            int n = 0;
            for (int k = 0; k < nt; ++k)
            {
                const int c = t[k];
                if (cnt[c] >= minshared && !(excludeself && c == ra))
                {
                    out[n] = c;
                    if (oc)
                        oc[n] = static_cast<int>(cnt[c]);
                    ++n;
                }
                cnt[c] = 0;
            }
        }
    }

    template<class CNT>
    o2m countingcomposition(o2m const &a, o2m const &b, int minshared, bool excludeself, o2m *counts)
    {
        o2m res;
        const int na = a.nelem;
        const int ncols = b.maxnode + 1;
        res.maxnode = b.maxnode;
        setsize(res, na);
        if (na == 0)
            return res;
        const int *as = a.start.begin(), *ad = a.data.begin(), *bs = b.start.begin(), *bd = b.data.begin();
        constexpr int CHUNK = 1024;
        const int nchunks = (na + CHUNK - 1) / CHUNK;
        std::unique_ptr<int[]> rowsz(new int[static_cast<std::size_t>(na)]);
#pragma omp parallel
        {
            std::unique_ptr<CNT[]> cnt(new CNT[static_cast<std::size_t>(ncols) + 1]());
            std::vector<int> scratch(1024);
#pragma omp for schedule(dynamic, 1)
            for (int c = 0; c < nchunks; ++c)
                dualcount<CNT>(c * CHUNK, std::min(na, (c + 1) * CHUNK), as, ad, bs, bd, minshared, excludeself,
                               cnt.get(), rowsz.get(), scratch);
        }
        const long long nnz = scan(rowsz.get(), na, res.start.begin());
        rowsz.reset();
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
#pragma omp parallel
        {
            std::unique_ptr<CNT[]> cnt(new CNT[static_cast<std::size_t>(ncols) + 1]());
            std::vector<int> scratch(1024);
#pragma omp for schedule(dynamic, 1)
            for (int c = 0; c < nchunks; ++c)
                dualfill<CNT>(c * CHUNK, std::min(na, (c + 1) * CHUNK), as, ad, bs, bd, minshared, excludeself,
                              cnt.get(), cs, cd, cc, scratch);
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

    // ------------------------------------------------------------------ union-find
    struct unionfind
    {
        std::unique_ptr<std::atomic<int>[]> parent;
        int n;

        explicit unionfind(int size) : parent(new std::atomic<int>[static_cast<std::size_t>(size)]), n(size)
        {
#pragma omp parallel for schedule(static)
            for (int i = 0; i < n; ++i)
                parent[static_cast<std::size_t>(i)].store(i, std::memory_order_relaxed);
        }

        int find(int x) noexcept
        {
            int p = parent[static_cast<std::size_t>(x)].load(std::memory_order_relaxed);
            while (p != x)
            {
                const int gp = parent[static_cast<std::size_t>(p)].load(std::memory_order_relaxed);
                parent[static_cast<std::size_t>(x)].store(gp, std::memory_order_relaxed); // path halving
                x = p;
                p = gp;
            }
            return x;
        }

        /** Links the roots, the larger index under the smaller (deterministic roots). */
        void unite(int a, int b) noexcept
        {
            for (;;)
            {
                int ra = find(a), rb = find(b);
                if (ra == rb)
                    return;
                if (ra < rb)
                    std::swap(ra, rb);
                int expected = ra;
                if (parent[static_cast<std::size_t>(ra)].compare_exchange_weak(expected, rb, std::memory_order_relaxed))
                    return;
            }
        }

        /** Full compression, then component ids in order of the smallest member. */
        int labels(seque<int> &label)
        {
            setsize(label, n);
#pragma omp parallel for schedule(static)
            for (int i = 0; i < n; ++i)
                label[i] = find(i);
            int ncomp = 0;
            for (int i = 0; i < n; ++i)
            {
                if (label[i] == i)
                    label[i] = -(++ncomp); // root: provisional negative id
            }
            // every root has a smaller index than its members (links go to the smaller index)
            for (int i = 0; i < n; ++i)
                if (label[i] >= 0)
                    label[i] = label[label[i]];
            for (int i = 0; i < n; ++i)
                label[i] = -label[i] - 1;
            return ncomp;
        }
    };

    /** Rows of a symmetric adjacency as a view. */
    inline crowview rowof(o2m const &g, int v) { return g[v]; }
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

seque<int> getkhopneighbourhood(m2m &rel, int element, int k, int minshared)
{
    synchronize(rel);
    seque<int> visited{element}, frontier{element};
    std::vector<unsigned char> seen;
    // a small open hash would do; the neighbourhoods are tiny, so sorted sequences suffice
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

int getcomponents(o2m const &graph, seque<int> &label)
{
    const int n = graph.nelem;
    unionfind uf(n);
#pragma omp parallel for schedule(dynamic, 4096)
    for (int u = 0; u < n; ++u)
        for (int v: graph[u])
            if (v > u)
                uf.unite(u, v);
    return uf.labels(label);
}

int getcomponents(m2m &rel, seque<int> &elementlabel, seque<int> &nodelabel)
{
    const int nel = rel.nfrome.nelem;
    const int nn = rel.nfrome.maxnode + 1;
    unionfind uf(nel + nn);
#pragma omp parallel for schedule(dynamic, 4096)
    for (int e = 0; e < nel; ++e)
        for (int node: rel.nfrome[e])
            uf.unite(e, nel + node);
    seque<int> all;
    const int ncomp = uf.labels(all);
    setsize(elementlabel, nel);
    setsize(nodelabel, nn);
    for (int e = 0; e < nel; ++e)
        elementlabel[e] = all[e];
    for (int v = 0; v < nn; ++v)
        nodelabel[v] = all[nel + v];
    return ncomp;
}

seque<int> getbfslevels(o2m const &graph, int source)
{
    const int n = graph.nelem;
    seque<int> level(n, -1);
    if (n == 0 || source < 0 || source >= n)
        return level;
    std::vector<int> frontier{source}, next;
    level[source] = 0;
    int d = 0;
    while (!frontier.empty())
    {
        ++d;
        next.clear();
        for (int u: frontier)
            for (int v: graph[u])
                if (level[v] < 0)
                {
                    level[v] = d;
                    next.push_back(v);
                }
        frontier.swap(next);
    }
    return level;
}

namespace
{
    /** BFS from s returning the levels and the last level's vertices (for the pseudo-peripheral search). */
    int bfslevels(o2m const &g, int s, std::vector<int> &level, std::vector<int> &last, std::vector<int> &order)
    {
        std::fill(level.begin(), level.end(), -1);
        order.clear();
        order.push_back(s);
        level[static_cast<std::size_t>(s)] = 0;
        std::size_t head = 0;
        int depth = 0;
        while (head < order.size())
        {
            const int u = order[head++];
            for (int v: g[u])
                if (level[static_cast<std::size_t>(v)] < 0)
                {
                    level[static_cast<std::size_t>(v)] = level[static_cast<std::size_t>(u)] + 1;
                    depth = level[static_cast<std::size_t>(v)];
                    order.push_back(v);
                }
        }
        last.clear();
        for (int u: order)
            if (level[static_cast<std::size_t>(u)] == depth)
                last.push_back(u);
        return depth;
    }

    /** George-Liu pseudo-peripheral vertex of the component containing s. */
    int pseudoperipheral(o2m const &g, int s, std::vector<int> &level, std::vector<int> &last, std::vector<int> &order)
    {
        int depth = bfslevels(g, s, level, last, order);
        for (int iter = 0; iter < 10; ++iter)
        {
            // the vertex of minimal degree in the last level
            int best = last[0];
            for (int u: last)
                if (g.nnodes(u) < g.nnodes(best))
                    best = u;
            const int d2 = bfslevels(g, best, level, last, order);
            if (d2 <= depth)
            {
                // restore the levels of s for the caller (not needed) and return best
                return best;
            }
            depth = d2;
            s = best;
        }
        return s;
    }
} // namespace

seque<int> getrcmorder(o2m const &graph)
{
    const int n = graph.nelem;
    seque<int> oldfromnew(n);
    if (n == 0)
        return oldfromnew;
    std::vector<unsigned char> visited(static_cast<std::size_t>(n), 0);
    std::vector<int> level(static_cast<std::size_t>(n)), last, order, nb;
    int pos = 0;
    for (int s0 = 0; s0 < n; ++s0)
    {
        if (visited[static_cast<std::size_t>(s0)])
            continue;
        const int s = pseudoperipheral(graph, s0, level, last, order);
        // Cuthill-McKee from s: BFS, neighbours in increasing degree
        const int first = pos;
        oldfromnew[pos++] = s;
        visited[static_cast<std::size_t>(s)] = 1;
        for (int head = first; head < pos; ++head)
        {
            const int u = oldfromnew[head];
            nb.clear();
            for (int v: graph[u])
                if (!visited[static_cast<std::size_t>(v)])
                {
                    visited[static_cast<std::size_t>(v)] = 1;
                    nb.push_back(v);
                }
            std::sort(nb.begin(), nb.end(), [&](int a, int b) {
                const int da = graph.nnodes(a), db = graph.nnodes(b);
                return da != db ? da < db : a < b;
            });
            for (int v: nb)
                oldfromnew[pos++] = v;
        }
        // reverse the component's segment
        std::reverse(oldfromnew.begin() + first, oldfromnew.begin() + pos);
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

int getcolouring(o2m const &graph, seque<int> &colour)
{
    const int n = graph.nelem;
    setsize(colour, n);
    for (int i = 0; i < n; ++i)
        colour[i] = -1;
    std::vector<int> used;
    int ncolours = 0;
    for (int u = 0; u < n; ++u)
    {
        used.assign(static_cast<std::size_t>(ncolours) + 1, 0);
        for (int v: graph[u])
            if (colour[v] >= 0)
                used[static_cast<std::size_t>(colour[v])] = 1;
        int c = 0;
        while (used[static_cast<std::size_t>(c)])
            ++c;
        colour[u] = c;
        ncolours = std::max(ncolours, c + 1);
    }
    return ncolours;
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
