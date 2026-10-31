#include "o2m.hpp"
#include <algorithm>
#include <cassert>
#include <climits>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <execution>
#include <memory>
#include <numeric>
#include <queue>
#include <stdexcept>
#include <type_traits>
#include <vector>
#ifdef _OPENMP
#include <omp.h>
#endif

// The bulk kernels are out-of-line functions on raw CSR arrays declared __restrict:
// the compiler then knows that the marker stores, the output stores and the
// connectivity loads touch disjoint memory, which is what a Fortran compiler
// assumes for dummy arrays, and pipelines the loops accordingly (the same loops
// written inline inside an OpenMP region run 2-3 times slower with icpx).
#define M2M_RESTRICT __restrict

// ======================================================================
// Helpers
// ======================================================================
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

    inline void checkentries(long long nnz)
    {
        if (nnz > INT_MAX)
            throw std::length_error("o2m: more than 2^31-1 entries");
    }

    /** Sorts a short row in place (insertion sort below 24 entries). */
    inline void sortrow(int *p, int n)
    {
        if (n < 24)
        {
            for (int i = 1; i < n; ++i)
            {
                int v = p[i];
                int j = i - 1;
                while (j >= 0 && p[j] > v)
                {
                    p[j + 1] = p[j];
                    --j;
                }
                p[j + 1] = v;
            }
        } else
        {
            std::sort(p, p + n);
        }
    }

    /** start[0..n] from sizes[0..n-1]; returns the total. Parallel in blocks. */
    long long exclusivescan(const int *sizes, int n, int *start)
    {
        const int nth = n > 200000 ? numthreads() : 1;
        std::vector<long long> blocksum(static_cast<std::size_t>(nth) + 1, 0);
#pragma omp parallel num_threads(nth)
        {
            const int t = threadnum();
            const int i0 = static_cast<int>(static_cast<long long>(n) * t / nth);
            const int i1 = static_cast<int>(static_cast<long long>(n) * (t + 1) / nth);
            long long acc = 0;
            for (int i = i0; i < i1; ++i)
                acc += sizes[i];
            blocksum[static_cast<std::size_t>(t) + 1] = acc;
#pragma omp barrier
#pragma omp single
            {
                for (int k = 0; k < nth; ++k)
                    blocksum[static_cast<std::size_t>(k) + 1] += blocksum[static_cast<std::size_t>(k)];
                checkentries(blocksum[static_cast<std::size_t>(nth)]);
            }
            long long run = blocksum[static_cast<std::size_t>(t)];
            for (int i = i0; i < i1; ++i)
            {
                start[i] = static_cast<int>(run);
                run += sizes[i];
            }
        }
        start[n] = static_cast<int>(blocksum[static_cast<std::size_t>(nth)]);
        return blocksum[static_cast<std::size_t>(nth)];
    }

    inline int maxof(crowview r, int current) noexcept
    {
        for (int i = 0; i < r.n; ++i)
            current = r.p[i] > current ? r.p[i] : current;
        return current;
    }

    /** Row length of a packed (JAG=false) or jagged (JAG=true) relation. */
    template<bool JAG>
    inline int rowlen(const int *s, const int *z, int e) noexcept
    {
        if constexpr (JAG)
            return z[e];
        else
            return s[e + 1] - s[e];
    }

    // ------------------------------------------------------------------ markers
    /** One bit per column; the owner clears what it set. */
    struct bitmark
    {
        using word = std::uint64_t;
        static constexpr bool stamped = false;
        std::unique_ptr<word[]> w;

        explicit bitmark(long long nbits)
        {
            const std::size_t nw = static_cast<std::size_t>((nbits + 63) / 64) + 1;
            w.reset(new word[nw]);
            std::memset(w.get(), 0, nw * sizeof(word));
        }

        static bool testandset(word *m, int i, int) noexcept
        {
            const word b = word(1) << (i & 63);
            word &x = m[static_cast<std::size_t>(i) >> 6];
            if (x & b)
                return true;
            x |= b;
            return false;
        }

        static void clear(word *m, int i) noexcept { m[static_cast<std::size_t>(i) >> 6] &= ~(word(1) << (i & 63)); }
    };

    /** One byte per column; the owner clears what it set. */
    struct bytemark
    {
        using word = unsigned char;
        static constexpr bool stamped = false;
        std::unique_ptr<word[]> w;

        explicit bytemark(long long n)
        {
            w.reset(new word[static_cast<std::size_t>(n) + 1]);
            std::memset(w.get(), 0, static_cast<std::size_t>(n) + 1);
        }

        static bool testandset(word *m, int i, int) noexcept
        {
            word &x = m[static_cast<std::size_t>(i)];
            if (x)
                return true;
            x = 1;
            return false;
        }

        static void clear(word *m, int i) noexcept { m[static_cast<std::size_t>(i)] = 0; }
    };

    /** One int per column stamped with the current row (as the Fortran routines): never cleared. */
    struct stampmark
    {
        using word = int;
        static constexpr bool stamped = true;
        std::unique_ptr<word[]> w;

        explicit stampmark(long long n)
        {
            w.reset(new word[static_cast<std::size_t>(n) + 1]);
            std::memset(w.get(), 0xFF, (static_cast<std::size_t>(n) + 1) * sizeof(word));
        }

        static bool testandset(word *m, int i, int row) noexcept
        {
            word &x = m[static_cast<std::size_t>(i)];
            if (x == row)
                return true;
            x = row;
            return false;
        }

        static void clear(word *, int) noexcept {}
    };

    /** Marker choice for a composition with ncols columns on nth threads (M2M_MUL_MODE=1 selects the
     *  row-stamped int marker of the Fortran routines, 2 the byte marker, 3 the bit marker). */
    inline int markermode(long long ncols, int nth)
    {
        if (const char *env = std::getenv("M2M_MUL_MODE"))
        {
            const int m = std::atoi(env);
            if (m >= 1 && m <= 3)
                return m;
        }
        // one byte per column and thread while that fits in 1 GB, one bit beyond
        const long long perthread = ncols + 1;
        if (perthread * nth <= (1024LL << 20))
            return 2;
        return 3;
    }

    /** Small open-addressing map key->value, reset by a generation stamp. */
    struct minihash
    {
        std::vector<int> key, val, stamp;
        int gen{0}, mask{0}, count{0};

        explicit minihash(int nslots = 256)
        {
            int m = 16;
            while (m < 2 * nslots)
                m <<= 1;
            key.assign(static_cast<std::size_t>(m), 0);
            val.assign(static_cast<std::size_t>(m), 0);
            stamp.assign(static_cast<std::size_t>(m), 0);
            mask = m - 1;
        }

        void reset() noexcept
        {
            ++gen;
            count = 0;
        }

        void grow()
        {
            std::vector<int> k2(key), v2(val), s2(stamp);
            const int m = static_cast<int>(key.size()) * 2;
            key.assign(static_cast<std::size_t>(m), 0);
            val.assign(static_cast<std::size_t>(m), 0);
            stamp.assign(static_cast<std::size_t>(m), 0);
            mask = m - 1;
            for (std::size_t i = 0; i < k2.size(); ++i)
            {
                if (s2[i] == gen)
                {
                    int h = static_cast<int>((static_cast<unsigned>(k2[i]) * 0x9E3779B1u) & static_cast<unsigned>(mask));
                    while (stamp[static_cast<std::size_t>(h)] == gen)
                        h = (h + 1) & mask;
                    stamp[static_cast<std::size_t>(h)] = gen;
                    key[static_cast<std::size_t>(h)] = k2[i];
                    val[static_cast<std::size_t>(h)] = v2[i];
                }
            }
        }

        int findorinsert(int k, int newval, bool &inserted)
        {
            if (2 * (count + 1) > static_cast<int>(key.size()))
                grow();
            int h = static_cast<int>((static_cast<unsigned>(k) * 0x9E3779B1u) & static_cast<unsigned>(mask));
            while (stamp[static_cast<std::size_t>(h)] == gen)
            {
                if (key[static_cast<std::size_t>(h)] == k)
                {
                    inserted = false;
                    return val[static_cast<std::size_t>(h)];
                }
                h = (h + 1) & mask;
            }
            stamp[static_cast<std::size_t>(h)] = gen;
            key[static_cast<std::size_t>(h)] = k;
            val[static_cast<std::size_t>(h)] = newval;
            ++count;
            inserted = true;
            return newval;
        }
    };

    // ------------------------------------------------------------------ transpose kernels
    template<bool JAG>
    void trcount(int e0, int e1, const int *M2M_RESTRICT s, const int *M2M_RESTRICT z, const int *M2M_RESTRICT d,
                 int *M2M_RESTRICT h)
    {
        for (int e = e0; e < e1; ++e)
        {
            const int *p = d + s[e];
            const int n = rowlen<JAG>(s, z, e);
            for (int k = 0; k < n; ++k)
                h[p[k]]++;
        }
    }

    template<bool JAG>
    void trcountatomic(int e0, int e1, const int *M2M_RESTRICT s, const int *M2M_RESTRICT z, const int *M2M_RESTRICT d,
                       int *M2M_RESTRICT h)
    {
        for (int e = e0; e < e1; ++e)
        {
            const int *p = d + s[e];
            const int n = rowlen<JAG>(s, z, e);
            for (int k = 0; k < n; ++k)
            {
#pragma omp atomic
                h[p[k]]++;
            }
        }
    }

    template<bool JAG>
    void trfill(int e0, int e1, const int *M2M_RESTRICT s, const int *M2M_RESTRICT z, const int *M2M_RESTRICT d,
                int *M2M_RESTRICT cursor, int *M2M_RESTRICT out)
    {
        for (int e = e0; e < e1; ++e)
        {
            const int *p = d + s[e];
            const int n = rowlen<JAG>(s, z, e);
            for (int k = 0; k < n; ++k)
                out[cursor[p[k]]++] = e;
        }
    }

    template<bool JAG>
    void trfillatomic(int e0, int e1, const int *M2M_RESTRICT s, const int *M2M_RESTRICT z, const int *M2M_RESTRICT d,
                      int *M2M_RESTRICT cursor, int *M2M_RESTRICT out)
    {
        for (int e = e0; e < e1; ++e)
        {
            const int *p = d + s[e];
            const int n = rowlen<JAG>(s, z, e);
            for (int k = 0; k < n; ++k)
            {
                int pos;
#pragma omp atomic capture
                {
                    pos = cursor[p[k]];
                    cursor[p[k]]++;
                }
                out[pos] = e;
            }
        }
    }

    void sortrows(int c0, int c1, const int *M2M_RESTRICT ts, int *M2M_RESTRICT td)
    {
        for (int c = c0; c < c1; ++c)
            sortrow(td + ts[c], ts[c + 1] - ts[c]);
    }

    // ------------------------------------------------------------------ composition kernels
    template<bool JA, bool JB, class MK>
    void composecount(int r0, int r1, const int *M2M_RESTRICT as, const int *M2M_RESTRICT az,
                      const int *M2M_RESTRICT ad, const int *M2M_RESTRICT bs, const int *M2M_RESTRICT bz,
                      const int *M2M_RESTRICT bd, int camax, typename MK::word *M2M_RESTRICT mk,
                      int *M2M_RESTRICT rowsz, std::vector<int> &scratch)
    {
        for (int ra = r0; ra < r1; ++ra)
        {
            const int *pa = ad + as[ra];
            const int nka = rowlen<JA>(as, az, ra);
            if constexpr (MK::stamped)
            {
                int ldg = 0;
                for (int ka = 0; ka < nka; ++ka)
                {
                    const int ca = pa[ka];
                    if (ca < 0 || ca > camax)
                        continue;
                    const int *pb = bd + bs[ca];
                    const int nkb = rowlen<JB>(bs, bz, ca);
                    for (int kb = 0; kb < nkb; ++kb)
                        ldg += !MK::testandset(mk, pb[kb], ra);
                }
                rowsz[ra] = ldg;
            } else
            {
                int bound = 0;
                for (int ka = 0; ka < nka; ++ka)
                {
                    const int ca = pa[ka];
                    if (ca >= 0 && ca <= camax)
                        bound += rowlen<JB>(bs, bz, ca);
                }
                if (static_cast<std::size_t>(bound) > scratch.size())
                    scratch.resize(static_cast<std::size_t>(2 * bound));
                int *M2M_RESTRICT t = scratch.data();
                int nt = 0;
                for (int ka = 0; ka < nka; ++ka)
                {
                    const int ca = pa[ka];
                    if (ca < 0 || ca > camax)
                        continue;
                    const int *pb = bd + bs[ca];
                    const int nkb = rowlen<JB>(bs, bz, ca);
                    for (int kb = 0; kb < nkb; ++kb)
                    {
                        const int cb = pb[kb];
                        if (!MK::testandset(mk, cb, ra))
                            t[nt++] = cb;
                    }
                }
                rowsz[ra] = nt;
                for (int k = 0; k < nt; ++k)
                    MK::clear(mk, t[k]);
            }
        }
    }

    template<bool JA, bool JB, class MK>
    void composefill(int r0, int r1, const int *M2M_RESTRICT as, const int *M2M_RESTRICT az,
                     const int *M2M_RESTRICT ad, const int *M2M_RESTRICT bs, const int *M2M_RESTRICT bz,
                     const int *M2M_RESTRICT bd, int camax, typename MK::word *M2M_RESTRICT mk,
                     const int *M2M_RESTRICT cs, int *M2M_RESTRICT cd)
    {
        for (int ra = r0; ra < r1; ++ra)
        {
            int *out = cd + cs[ra];
            int len = 0;
            const int *pa = ad + as[ra];
            const int nka = rowlen<JA>(as, az, ra);
            for (int ka = 0; ka < nka; ++ka)
            {
                const int ca = pa[ka];
                if (ca < 0 || ca > camax)
                    continue;
                const int *pb = bd + bs[ca];
                const int nkb = rowlen<JB>(bs, bz, ca);
                for (int kb = 0; kb < nkb; ++kb)
                {
                    const int cb = pb[kb];
                    if (!MK::testandset(mk, cb, ra))
                        out[len++] = cb;
                }
            }
            if constexpr (!MK::stamped)
                for (int k = 0; k < len; ++k)
                    MK::clear(mk, out[k]);
        }
    }

    template<bool JA, bool JB, class MK>
    o2m compose(o2m const &rela, o2m const &relb)
    {
        o2m relc;
        const int na = rela.nelem;
        relc.maxnode = relb.maxnode;
        setsize(relc, na);
        if (na == 0)
            return relc;
        const long long ncols = static_cast<long long>(relb.maxnode) + 1;
        const int camax = relb.nelem - 1;
        const int *as = rela.start.begin(), *az = rela.size.begin(), *ad = rela.data.begin();
        const int *bs = relb.start.begin(), *bz = relb.size.begin(), *bd = relb.data.begin();
        constexpr int CHUNK = 1024;
        const int nchunks = (na + CHUNK - 1) / CHUNK;
        std::unique_ptr<int[]> rowsz(new int[static_cast<std::size_t>(na)]);
        const bool timing = std::getenv("M2M_TIMING") != nullptr;
        double t0 = 0, t1 = 0, t2 = 0;
#ifdef _OPENMP
        if (timing) t0 = omp_get_wtime();
#endif
#pragma omp parallel
        {
            MK mark(ncols);
            std::vector<int> scratch(MK::stamped ? 0 : 1024);
#pragma omp for schedule(dynamic, 1)
            for (int c = 0; c < nchunks; ++c)
                composecount<JA, JB, MK>(c * CHUNK, std::min(na, (c + 1) * CHUNK), as, az, ad, bs, bz, bd, camax,
                                         mark.w.get(), rowsz.get(), scratch);
        }
#ifdef _OPENMP
        if (timing) t1 = omp_get_wtime();
#endif
        const long long nnz = exclusivescan(rowsz.get(), na, relc.start.begin());
        rowsz.reset();
        setsizeexact(relc.data, nnz);
        const int *cs = relc.start.begin();
        int *cd = relc.data.begin();
#pragma omp parallel
        {
            MK mark(ncols);
#pragma omp for schedule(dynamic, 1)
            for (int c = 0; c < nchunks; ++c)
                composefill<JA, JB, MK>(c * CHUNK, std::min(na, (c + 1) * CHUNK), as, az, ad, bs, bz, bd, camax,
                                        mark.w.get(), cs, cd);
        }
#ifdef _OPENMP
        if (timing)
        {
            t2 = omp_get_wtime();
            std::fprintf(stderr, "compose: count %.3f s, scan+alloc+fill %.3f s (marker %s)\n", t1 - t0, t2 - t1,
                         MK::stamped ? "stamp" : (sizeof(typename MK::word) == 1 ? "byte" : "bit"));
        }
#endif
        return relc;
    }

    template<class MK>
    o2m composejag(o2m const &rela, o2m const &relb)
    {
        const bool ja = rela.isjagged(), jb = relb.isjagged();
        if (!ja && !jb)
            return compose<false, false, MK>(rela, relb);
        if (ja && !jb)
            return compose<true, false, MK>(rela, relb);
        if (!ja && jb)
            return compose<false, true, MK>(rela, relb);
        return compose<true, true, MK>(rela, relb);
    }

    // ------------------------------------------------------------------ row copy kernel
    template<bool JAG>
    void copyrows(int i0, int i1, const int *M2M_RESTRICT ord, const int *M2M_RESTRICT ss, const int *M2M_RESTRICT sz,
                  const int *M2M_RESTRICT sd, const int *M2M_RESTRICT ds, int *M2M_RESTRICT dd)
    {
        for (int i = i0; i < i1; ++i)
        {
            const int e = ord[i];
            const int n = rowlen<JAG>(ss, sz, e);
            if (n > 0)
                std::memcpy(dd + ds[i], sd + ss[e], static_cast<size_t>(n) * sizeof(int));
        }
    }

    /** Materializes the size array (the relation becomes jagged). */
    void materialize(o2m &rel)
    {
        if (rel.size.size != 0 || rel.nelem == 0)
            return;
        setsize(rel.size, rel.nelem);
        for (int i = 0; i < rel.nelem; ++i)
            rel.size[i] = rel.start[i + 1] - rel.start[i];
    }
} // namespace

// ======================================================================
// Basic maintenance
// ======================================================================

void setsize(o2m &rel, int nelem)
{
    const int old = rel.nelem;
    if (nelem == old)
        return;
    if (nelem > old)
    {
        const bool jag = rel.isjagged();
        const int tail = jag ? rel.data.size : rel.start[old];
        setsize(rel.start, nelem + 1);
        for (int i = old + 1; i <= nelem; ++i)
            rel.start[i] = tail;
        if (jag)
        {
            rel.start[old] = tail;
            setsize(rel.size, nelem);
            for (int i = old; i < nelem; ++i)
                rel.size[i] = 0;
        }
    } else
    {
        setsize(rel.start, nelem + 1);
        if (rel.isjagged())
            setsize(rel.size, nelem);
        else
            setsize(rel.data, rel.start[nelem]);
    }
    rel.nelem = nelem;
}

void reserve(o2m &rel, int nelem, long long nentries)
{
    checkentries(nentries);
    const int nr = rel.start.size, nd = rel.data.size;
    if (nelem + 1 > nr)
    {
        setsize(rel.start, nelem + 1);
        setsize(rel.start, nr);
    }
    if (nentries > nd)
    {
        setsize(rel.data, static_cast<int>(nentries));
        setsize(rel.data, nd);
    }
}

int appendelement(o2m &rel, crowview nodes)
{
    const int n = nodes.n;
    if (n > 1)
    {
        bool repeated = false;
        if (n <= 16)
        {
            for (int i = 1; i < n && !repeated; ++i)
                for (int j = 0; j < i; ++j)
                    if (nodes.p[i] == nodes.p[j])
                    {
                        repeated = true;
                        break;
                    }
        } else
        {
            seque<int> temp = nodes;
            sortrow(temp.begin(), n);
            for (int i = 1; i < n; ++i)
                if (temp[i] == temp[i - 1])
                    repeated = true;
        }
        if (repeated)
            throw std::runtime_error("Repeated nodes detected in the input.");
    }
    seque<int> saved;
    const int *src = nodes.p;
    if (n > 0 && src >= rel.data.begin() && src < rel.data.begin() + rel.data.size)
    {
        saved = nodes;
        src = saved.begin();
    }
    const int e = rel.nelem;
    const int pos = rel.data.size;
    setsize(rel.data, pos + n);
    if (n > 0)
        std::memcpy(rel.data.begin() + pos, src, static_cast<size_t>(n) * sizeof(int));
    setsize(rel.start, e + 2);
    rel.start[e] = pos;
    rel.start[e + 1] = pos + n;
    if (rel.isjagged())
    {
        setsize(rel.size, e + 1);
        rel.size[e] = n;
    }
    rel.nelem = e + 1;
    rel.maxnode = maxof(nodes, rel.maxnode);
    return e;
}

void setrow(o2m &rel, int element, crowview nodes)
{
    assert(element >= 0 && element < rel.nelem);
    const int n = nodes.n;
    const int n0 = rel.nnodes(element);
    if (n <= n0)
    {
        if (n > 0)
            std::memmove(rel.data.begin() + rel.start[element], nodes.p, static_cast<size_t>(n) * sizeof(int));
        if (n < n0)
        {
            materialize(rel);
            rel.size[element] = n;
        }
    } else
    {
        seque<int> saved;
        const int *src = nodes.p;
        if (src >= rel.data.begin() && src < rel.data.begin() + rel.data.size)
        {
            saved = nodes;
            src = saved.begin();
        }
        if (!rel.isjagged() && element == rel.nelem - 1)
        {
            // the last row of a packed relation grows in place at the end of the pool
            const int pos = rel.start[element];
            setsize(rel.data, pos + n);
            std::memcpy(rel.data.begin() + pos, src, static_cast<size_t>(n) * sizeof(int));
            rel.start[element + 1] = pos + n;
        } else
        {
            materialize(rel);
            const int pos = rel.data.size;
            setsize(rel.data, pos + n);
            std::memcpy(rel.data.begin() + pos, src, static_cast<size_t>(n) * sizeof(int));
            rel.start[element] = pos;
            rel.size[element] = n;
        }
    }
    rel.maxnode = maxof(nodes, rel.maxnode);
}

void clearrow(o2m &rel, int element)
{
    assert(element >= 0 && element < rel.nelem);
    if (rel.nnodes(element) == 0)
        return;
    materialize(rel);
    rel.size[element] = 0;
}

void appendtorow(o2m &rel, int element, int node)
{
    assert(element >= 0 && element < rel.nelem);
    const bool jag = rel.isjagged();
    const int end = rel.start[element] + rel.nnodes(element);
    if (end == rel.data.size && (jag || element == rel.nelem - 1))
    {
        setsize(rel.data, end + 1);
        rel.data[end] = node;
        if (jag)
            rel.size[element]++;
        else
            rel.start[element + 1] = end + 1;
    } else
    {
        seque<int> row = rel[element];
        append(row, node);
        setrow(rel, element, row);
    }
    if (node > rel.maxnode)
        rel.maxnode = node;
}

o2m &operator<<(o2m &rel, std::initializer_list<int> nodes)
{
    appendelement(rel, crowview(nodes));
    return rel;
}

long long getnnz(o2m const &rel)
{
    if (!rel.isjagged())
        return rel.start[rel.nelem];
    long long nnz = 0;
    const int *z = rel.size.begin();
    const int n = rel.nelem;
#pragma omp parallel for schedule(static) reduction(+ : nnz) if (n > 100000)
    for (int i = 0; i < n; ++i)
        nnz += z[i];
    return nnz;
}

bool ispacked(o2m const &rel) { return !rel.isjagged(); }

o2m getrows(o2m const &rel, seque<int> const &order)
{
    o2m res;
    const int n = order.size;
    setsize(res, n);
    std::unique_ptr<int[]> sizes(new int[static_cast<std::size_t>(std::max(n, 1))]);
    for (int i = 0; i < n; ++i)
    {
        const int e = order[i];
        if (e < 0 || e >= rel.nelem)
            throw std::out_of_range("o2m: row index out of range");
        sizes[static_cast<std::size_t>(i)] = rel.nnodes(e);
    }
    const long long nnz = exclusivescan(sizes.get(), n, res.start.begin());
    sizes.reset();
    setsizeexact(res.data, nnz);
    const int *ord = order.begin();
    const int *ss = rel.start.begin(), *sz = rel.size.begin(), *sd = rel.data.begin();
    const int *ds = res.start.begin();
    int *dd = res.data.begin();
    constexpr int CHUNK = 4096;
    const int nchunks = (n + CHUNK - 1) / CHUNK;
    const bool jag = rel.isjagged();
#pragma omp parallel for schedule(dynamic, 1) if (n > 50000)
    for (int c = 0; c < nchunks; ++c)
    {
        const int i0 = c * CHUNK, i1 = std::min(n, i0 + CHUNK);
        if (jag)
            copyrows<true>(i0, i1, ord, ss, sz, sd, ds, dd);
        else
            copyrows<false>(i0, i1, ord, ss, sz, sd, ds, dd);
    }
    res.maxnode = rel.maxnode;
    return res;
}

void pack(o2m &rel)
{
    if (!rel.isjagged())
        return;
    seque<int> identity(rel.nelem);
    std::iota(identity.begin(), identity.end(), 0);
    o2m packed = getrows(rel, identity);
    rel.start = std::move(packed.start);
    rel.data = std::move(packed.data);
    setsize(rel.size, 0);
}

void updatemaxnode(o2m &rel)
{
    int m = -1;
    const int n = rel.nelem;
#pragma omp parallel for schedule(static) reduction(max : m) if (n > 50000)
    for (int i = 0; i < n; ++i)
        m = maxof(rel[i], m);
    rel.maxnode = m;
}

// ======================================================================
// Comparison, printing, hashing
// ======================================================================

bool operator==(o2m const &a, o2m const &b)
{
    if (a.nelem != b.nelem || a.maxnode != b.maxnode)
        return false;
    for (int i = 0; i < a.nelem; ++i)
        if (a[i] != b[i])
            return false;
    return true;
}

bool operator<(o2m const &a, o2m const &b)
{
    const int n = std::min(a.nelem, b.nelem);
    for (int i = 0; i < n; ++i)
    {
        if (a[i] != b[i])
            return lexicompare(a[i], b[i]);
    }
    return a.nelem < b.nelem;
}

std::ostream &operator<<(std::ostream &os, o2m const &rel)
{
    os << rel.nelem << ' ' << rel.maxnode << '\n';
    for (int i = 0; i < rel.nelem; ++i)
        os << rel[i] << '\n';
    return os;
}

std::istream &operator>>(std::istream &is, o2m &rel)
{
    int n = 0, mx = 0;
    is >> n >> mx;
    rel = o2m();
    seque<int> row;
    for (int i = 0; i < n; ++i)
    {
        int k = 0;
        is >> k;
        setsize(row, k);
        for (int j = 0; j < k; ++j)
            is >> row[j];
        const int pos = rel.data.size;
        setsize(rel.data, pos + k);
        if (k > 0)
            std::memcpy(rel.data.begin() + pos, row.begin(), static_cast<size_t>(k) * sizeof(int));
        setsize(rel.start, i + 2);
        rel.start[i + 1] = pos + k;
        rel.nelem = i + 1;
    }
    rel.maxnode = mx;
    return is;
}

std::size_t hash_value(o2m const &rel)
{
    std::size_t h = static_cast<std::size_t>(rel.nelem) * 1000003u ^ static_cast<std::size_t>(rel.maxnode);
    for (int i = 0; i < rel.nelem; ++i)
    {
        crowview r = rel[i];
        h ^= static_cast<std::size_t>(r.n) + 0x9e3779b97f4a7c15ULL + (h << 6) + (h >> 2);
        for (int k = 0; k < r.n; ++k)
            h ^= static_cast<std::size_t>(r.p[k]) + 0x9e3779b97f4a7c15ULL + (h << 6) + (h >> 2);
    }
    return h;
}

// ======================================================================
// Ordering and duplicates
// ======================================================================

namespace
{
    struct key2
    {
        std::uint64_t k;
        int i;
    };

    struct key4
    {
        std::uint64_t k0, k1;
        int i;
    };

    inline std::uint64_t packpair(crowview r, int a, int b) noexcept
    {
        // entries are non-negative; a missing entry (shorter row) packs as 0 and
        // the length decides, so a shorter prefix sorts first
        const std::uint64_t x = a < r.n ? static_cast<std::uint64_t>(r.p[a]) + 1 : 0;
        const std::uint64_t y = b < r.n ? static_cast<std::uint64_t>(r.p[b]) + 1 : 0;
        return (x << 32) | y;
    }
} // namespace

seque<int> getorder(o2m const &rel)
{
    const int n = rel.nelem;
    seque<int> idx(n);
    if (n == 0)
        return idx;
    int maxlen = 0;
    bool nonneg = true;
    for (int i = 0; i < n; ++i)
    {
        crowview r = rel[i];
        maxlen = std::max(maxlen, r.n);
        for (int k = 0; k < r.n; ++k)
            nonneg = nonneg && r.p[k] >= 0 && r.p[k] < INT_MAX;
    }
    const bool par = n > 50000 && numthreads() > 1;
    if (nonneg && maxlen <= 2)
    {
        // packed 64-bit keys (edges): one comparison per pair
        std::vector<key2> keys(static_cast<std::size_t>(n));
#pragma omp parallel for schedule(static) if (par)
        for (int i = 0; i < n; ++i)
            keys[static_cast<std::size_t>(i)] = key2{packpair(rel[i], 0, 1), i};
        auto cmp = [](key2 const &a, key2 const &b) { return a.k < b.k || (a.k == b.k && a.i < b.i); };
        if (par)
            std::sort(std::execution::par, keys.begin(), keys.end(), cmp);
        else
            std::sort(keys.begin(), keys.end(), cmp);
        for (int i = 0; i < n; ++i)
            idx[i] = keys[static_cast<std::size_t>(i)].i;
        return idx;
    }
    if (nonneg && maxlen <= 4)
    {
        std::vector<key4> keys(static_cast<std::size_t>(n));
#pragma omp parallel for schedule(static) if (par)
        for (int i = 0; i < n; ++i)
            keys[static_cast<std::size_t>(i)] = key4{packpair(rel[i], 0, 1), packpair(rel[i], 2, 3), i};
        auto cmp = [](key4 const &a, key4 const &b)
        {
            if (a.k0 != b.k0)
                return a.k0 < b.k0;
            if (a.k1 != b.k1)
                return a.k1 < b.k1;
            return a.i < b.i;
        };
        if (par)
            std::sort(std::execution::par, keys.begin(), keys.end(), cmp);
        else
            std::sort(keys.begin(), keys.end(), cmp);
        for (int i = 0; i < n; ++i)
            idx[i] = keys[static_cast<std::size_t>(i)].i;
        return idx;
    }
    std::iota(idx.begin(), idx.end(), 0);
    auto cmp = [&rel](int i, int j) { return lexicompare(rel[i], rel[j]); };
    if (par)
        std::stable_sort(std::execution::par, idx.begin(), idx.end(), cmp);
    else
        std::stable_sort(idx.begin(), idx.end(), cmp);
    return idx;
}

seque<int> getindicesofduplicates(o2m const &rel, seque<int> const &order)
{
    seque<int> dup;
    const int n = order.size;
    for (int i = 1; i < n; ++i)
    {
        if (rel[order[i]] == rel[order[i - 1]])
            append(dup, order[i]);
    }
    return dup;
}

seque<int> getduplicates(o2m const &rel)
{
    seque<int> dup = getindicesofduplicates(rel, getorder(rel));
    setordered(dup);
    return dup;
}

void indicesfromorder(o2m const &rel, seque<int> const &order, seque<int> &newfromold, seque<int> &oldfromnew)
{
    const int n = order.size;
    setsize(newfromold, n);
    setsize(oldfromnew, 0);
    if (n == 0)
        return;
    int unique = -1;
    for (int i = 0; i < n; ++i)
    {
        const int e = order[i];
        if (i == 0 || rel[e] != rel[order[i - 1]])
        {
            ++unique;
            append(oldfromnew, e);
        }
        newfromold[e] = unique;
    }
}

o2m geto2mfromsequence(seque<int> const &other)
{
    o2m ret;
    const int n = other.size;
    setsize(ret, n);
    setsizeexact(ret.data, n);
    for (int e = 0; e < n; ++e)
    {
        ret.start[e] = e;
        ret.data[e] = e;
    }
    ret.start[n] = n;
    ret.maxnode = n > 0 ? n - 1 : 0;
    return ret;
}

// ======================================================================
// Transpose
// ======================================================================

namespace
{
    template<bool JAG>
    o2m transpose(o2m const &rel)
    {
        o2m relt;
        if (rel.nelem == 0)
            return relt;
        const int n = rel.nelem;
        const int ncols = rel.maxnode + 1;
        const long long nnz = getnnz(rel);
        checkentries(nnz);
        relt.maxnode = n - 1;
        setsize(relt, ncols);
        setsizeexact(relt.data, nnz);
        const int *s = rel.start.begin(), *z = rel.size.begin(), *d = rel.data.begin();
        int *ts = relt.start.begin();
        int *td = relt.data.begin();
        const int nth = numthreads();
        int mode = 0; // 0: automatic, 1: per-thread histograms, 2: atomic cursors and row sort
        if (const char *env = std::getenv("M2M_TR_MODE"))
            mode = std::atoi(env);
        const bool histogram = mode == 1 || (mode == 0 && (nth == 1 || static_cast<long long>(nth) * ncols <= (1LL << 26)));
        if (histogram)
        {
            // Counting fill with per-thread histograms: thread t owns the elements
            // [n*t/nth, n*(t+1)/nth); the prefix over threads gives every thread its own
            // cursor in each column, so the fill needs no atomics and every row of the
            // transpose comes out in ascending element order. With one thread this is
            // the serial stable counting fill of the Fortran routine.
            std::unique_ptr<int[]> hist(new int[static_cast<std::size_t>(nth) * ncols]);
#pragma omp parallel num_threads(nth)
            {
                const int t = threadnum();
                int *h = hist.get() + static_cast<std::size_t>(t) * ncols;
                std::memset(h, 0, static_cast<std::size_t>(ncols) * sizeof(int));
                const int e0 = static_cast<int>(static_cast<long long>(n) * t / nth);
                const int e1 = static_cast<int>(static_cast<long long>(n) * (t + 1) / nth);
                trcount<JAG>(e0, e1, s, z, d, h);
#pragma omp barrier
                const int c0 = static_cast<int>(static_cast<long long>(ncols) * t / nth);
                const int c1 = static_cast<int>(static_cast<long long>(ncols) * (t + 1) / nth);
                for (int c = c0; c < c1; ++c)
                {
                    int sum = 0;
                    for (int tt = 0; tt < nth; ++tt)
                    {
                        int *g = hist.get() + static_cast<std::size_t>(tt) * ncols + c;
                        const int v = *g;
                        *g = sum;
                        sum += v;
                    }
                    ts[c + 1] = sum; // row size, scanned below
                }
#pragma omp barrier
#pragma omp single
                {
                    ts[0] = 0;
                    for (int c = 0; c < ncols; ++c)
                        ts[c + 1] += ts[c];
                }
                for (int c = 0; c < ncols; ++c)
                    h[c] += ts[c];
                trfill<JAG>(e0, e1, s, z, d, h, td);
            }
        } else
        {
            std::unique_ptr<int[]> cnt(new int[static_cast<std::size_t>(ncols)]);
            int *c = cnt.get();
#pragma omp parallel for schedule(static)
            for (int i = 0; i < ncols; ++i)
                c[i] = 0;
            constexpr int CHUNK = 4096;
            const int nchunks = (n + CHUNK - 1) / CHUNK;
#pragma omp parallel for schedule(static)
            for (int k = 0; k < nchunks; ++k)
                trcountatomic<JAG>(k * CHUNK, std::min(n, (k + 1) * CHUNK), s, z, d, c);
            exclusivescan(c, ncols, ts);
#pragma omp parallel for schedule(static)
            for (int i = 0; i < ncols; ++i)
                c[i] = ts[i];
#pragma omp parallel for schedule(static)
            for (int k = 0; k < nchunks; ++k)
                trfillatomic<JAG>(k * CHUNK, std::min(n, (k + 1) * CHUNK), s, z, d, c, td);
            const int cchunks = (ncols + 8191) / 8192;
#pragma omp parallel for schedule(dynamic, 1)
            for (int k = 0; k < cchunks; ++k)
                sortrows(k * 8192, std::min(ncols, (k + 1) * 8192), ts, td);
        }
        return relt;
    }
} // namespace

o2m Tr(o2m const &rel)
{
    return rel.isjagged() ? transpose<true>(rel) : transpose<false>(rel);
}

// ======================================================================
// Composition
// ======================================================================

o2m operator*(o2m const &rela, o2m const &relb)
{
    switch (markermode(static_cast<long long>(relb.maxnode) + 1, numthreads()))
    {
        case 1:
            return composejag<stampmark>(rela, relb);
        case 2:
            return composejag<bytemark>(rela, relb);
        default:
            return composejag<bitmark>(rela, relb);
    }
}

o2m operator*(o2m const &rela, seque<int> const &vec)
{
    o2m temp = geto2mfromsequence(vec);
    return rela * temp;
}

// ======================================================================
// Set algebra (row-wise)
// ======================================================================

o2m operator+(o2m const &rela, o2m const &relb)
{
    o2m relc;
    const int na = rela.nelem, nb = relb.nelem;
    const int n = std::max(na, nb);
    setsize(relc, n);
    relc.maxnode = std::max(rela.maxnode, relb.maxnode);
    if (n == 0)
        return relc;
    const long long nbits = static_cast<long long>(relc.maxnode) + 1;
    std::unique_ptr<int[]> rowsz(new int[static_cast<std::size_t>(n)]);
#pragma omp parallel
    {
        bitmark mark(nbits);
        std::uint64_t *const mp = mark.w.get();
        std::vector<int> touched;
        touched.reserve(512);
#pragma omp for schedule(dynamic, 1024)
        for (int e = 0; e < n; ++e)
        {
            touched.clear();
            if (e < na)
                for (int v: rela[e])
                    if (!bitmark::testandset(mp, v, 0))
                        touched.push_back(v);
            if (e < nb)
                for (int v: relb[e])
                    if (!bitmark::testandset(mp, v, 0))
                        touched.push_back(v);
            rowsz[static_cast<std::size_t>(e)] = static_cast<int>(touched.size());
            for (int t: touched)
                bitmark::clear(mp, t);
        }
    }
    const long long nnz = exclusivescan(rowsz.get(), n, relc.start.begin());
    rowsz.reset();
    setsizeexact(relc.data, nnz);
    int *cd = relc.data.begin();
    const int *cs = relc.start.begin();
#pragma omp parallel
    {
        bitmark mark(nbits);
        std::uint64_t *const mp = mark.w.get();
#pragma omp for schedule(dynamic, 1024)
        for (int e = 0; e < n; ++e)
        {
            int *out = cd + cs[e];
            int len = 0;
            if (e < na)
                for (int v: rela[e])
                    if (!bitmark::testandset(mp, v, 0))
                        out[len++] = v;
            if (e < nb)
                for (int v: relb[e])
                    if (!bitmark::testandset(mp, v, 0))
                        out[len++] = v;
            for (int k = 0; k < len; ++k)
                bitmark::clear(mp, out[k]);
        }
    }
    return relc;
}

o2m operator||(o2m const &rela, o2m const &relb) { return rela + relb; }

namespace
{
    inline void sortedcopy(crowview r, std::vector<int> &buf)
    {
        buf.resize(static_cast<std::size_t>(r.n));
        if (r.n > 0)
            std::memcpy(buf.data(), r.p, static_cast<size_t>(r.n) * sizeof(int));
        sortrow(buf.data(), r.n);
    }

    template<bool INTERSECT>
    o2m rowsetop(o2m const &rela, o2m const &relb)
    {
        o2m relc;
        const int na = rela.nelem, nb = relb.nelem;
        const int n = INTERSECT ? std::min(na, nb) : na;
        setsize(relc, n);
        relc.maxnode = std::max(rela.maxnode, relb.maxnode);
        if (n == 0)
            return relc;
        std::unique_ptr<int[]> rowsz(new int[static_cast<std::size_t>(n)]);
#pragma omp parallel
        {
            std::vector<int> ta, tb, out;
#pragma omp for schedule(dynamic, 1024)
            for (int e = 0; e < n; ++e)
            {
                if (!INTERSECT && e >= nb)
                {
                    rowsz[static_cast<std::size_t>(e)] = rela.nnodes(e);
                    continue;
                }
                sortedcopy(rela[e], ta);
                sortedcopy(relb[e], tb);
                out.resize(ta.size() + tb.size());
                auto endit = INTERSECT
                                 ? std::set_intersection(ta.begin(), ta.end(), tb.begin(), tb.end(), out.begin())
                                 : std::set_difference(ta.begin(), ta.end(), tb.begin(), tb.end(), out.begin());
                rowsz[static_cast<std::size_t>(e)] = static_cast<int>(endit - out.begin());
            }
        }
        const long long nnz = exclusivescan(rowsz.get(), n, relc.start.begin());
        rowsz.reset();
        setsizeexact(relc.data, nnz);
        int *cd = relc.data.begin();
        const int *cs = relc.start.begin();
#pragma omp parallel
        {
            std::vector<int> ta, tb;
#pragma omp for schedule(dynamic, 1024)
            for (int e = 0; e < n; ++e)
            {
                int *dst = cd + cs[e];
                if (!INTERSECT && e >= nb)
                {
                    crowview r = rela[e];
                    if (r.n > 0)
                        std::memcpy(dst, r.p, static_cast<size_t>(r.n) * sizeof(int));
                    continue;
                }
                sortedcopy(rela[e], ta);
                sortedcopy(relb[e], tb);
                if (INTERSECT)
                    std::set_intersection(ta.begin(), ta.end(), tb.begin(), tb.end(), dst);
                else
                    std::set_difference(ta.begin(), ta.end(), tb.begin(), tb.end(), dst);
            }
        }
        return relc;
    }
} // namespace

o2m operator&&(o2m const &rela, o2m const &relb) { return rowsetop<true>(rela, relb); }

o2m operator-(o2m const &rela, o2m const &relb) { return rowsetop<false>(rela, relb); }

seque<int> getintersection(crowview a, crowview b)
{
    seque<int> res(a.n < b.n ? a.n : b.n);
    int i = 0, j = 0, k = 0;
    while (i < a.n && j < b.n)
    {
        if (a.p[i] < b.p[j])
            ++i;
        else if (b.p[j] < a.p[i])
            ++j;
        else
        {
            res[k++] = a.p[i];
            ++i;
            ++j;
        }
    }
    setsize(res, k);
    return res;
}

seque<int> getunion(crowview a, crowview b)
{
    seque<int> res(a.n + b.n);
    auto endit = std::set_union(a.begin(), a.end(), b.begin(), b.end(), res.begin());
    setsize(res, static_cast<int>(endit - res.begin()));
    return res;
}

seque<int> getdifference(crowview a, crowview b)
{
    seque<int> res(a.n);
    auto endit = std::set_difference(a.begin(), a.end(), b.begin(), b.end(), res.begin());
    setsize(res, static_cast<int>(endit - res.begin()));
    return res;
}

// ======================================================================
// Topological order
// ======================================================================

seque<int> gettoporder(o2m const &rel)
{
    seque<int> order;
    const int n = rel.nelem;
    std::vector<int> indegree(static_cast<std::size_t>(std::max(n, rel.maxnode + 1)), 0);
    for (int i = 0; i < n; ++i)
        for (int nbr: rel[i])
            ++indegree[static_cast<std::size_t>(nbr)];
    std::queue<int> q;
    for (int i = 0; i < n; ++i)
        if (indegree[static_cast<std::size_t>(i)] == 0)
            q.push(i);
    while (!q.empty())
    {
        const int cur = q.front();
        q.pop();
        append(order, cur);
        for (int nbr: rel[cur])
            if (--indegree[static_cast<std::size_t>(nbr)] == 0 && nbr < n)
                q.push(nbr);
    }
    if (order.size != n)
        throw std::runtime_error("The relation contains cycles, topological sort not possible.");
    return order;
}

// ======================================================================
// Compression, permutation, local positions, cliques
// ======================================================================

void hidden::compresselements(o2m &rel, seque<int> const &oldelementfromnew)
{
    o2m res = getrows(rel, oldelementfromnew);
    rel.start = std::move(res.start);
    rel.data = std::move(res.data);
    setsize(rel.size, 0);
    rel.nelem = oldelementfromnew.size;
    updatemaxnode(rel);
}

void hidden::permutenodes(o2m &rel, seque<int> const &newnodefromold)
{
    const int n = rel.nelem;
    const int m = newnodefromold.size;
    const int *map = newnodefromold.begin();
    int bad = 0;
#pragma omp parallel for schedule(static) reduction(+ : bad) if (n > 50000)
    for (int e = 0; e < n; ++e)
    {
        rowview r = rel[e];
        for (int k = 0; k < r.n; ++k)
        {
            const int v = r.p[k];
            if (v < 0 || v >= m)
                bad++;
            else
                r.p[k] = map[v];
        }
    }
    if (bad)
        throw std::out_of_range("permutenodes: node index outside the map");
    updatemaxnode(rel);
}

o2m hidden::getshape(o2m const &shape)
{
    o2m res;
    const int n = shape.nelem;
    setsize(res, n);
    if (!shape.isjagged())
    {
        std::memcpy(res.start.begin(), shape.start.begin(), static_cast<size_t>(n + 1) * sizeof(int));
    } else
    {
        exclusivescan(shape.size.begin(), n, res.start.begin());
    }
    setsizeexact(res.data, res.start[n]);
    res.maxnode = shape.maxnode;
    return res;
}

namespace
{
    template<bool JN, bool JE>
    int locate(int n0, int n1, const int *M2M_RESTRICT es, const int *M2M_RESTRICT ez, const int *M2M_RESTRICT ed,
               const int *M2M_RESTRICT ns, const int *M2M_RESTRICT nz, const int *M2M_RESTRICT nd,
               const int *M2M_RESTRICT nls, int *M2M_RESTRICT nld, const int *M2M_RESTRICT els,
               int *M2M_RESTRICT eld)
    {
        int bad = 0;
        for (int node = n0; node < n1; ++node)
        {
            const int *els_ = ed + es[node];
            const int ne = rowlen<JE>(es, ez, node);
            for (int k = 0; k < ne; ++k)
            {
                const int e = els_[k];
                const int *nodes = nd + ns[e];
                const int nn = rowlen<JN>(ns, nz, e);
                int i = 0;
                while (i < nn && nodes[i] != node)
                    ++i;
                if (i == nn)
                {
                    bad++;
                    continue;
                }
                if (nld)
                    nld[nls[node] + k] = i;
                if (eld)
                    eld[els[e] + i] = k;
            }
        }
        return bad;
    }

    void locations(o2m const &ne, o2m const &en, o2m *nodeloc, o2m *elementloc)
    {
        if (nodeloc)
            *nodeloc = hidden::getshape(en);
        if (elementloc)
            *elementloc = hidden::getshape(ne);
        const int nn = en.nelem;
        const int *es = en.start.begin(), *ez = en.size.begin(), *ed = en.data.begin();
        const int *ns = ne.start.begin(), *nz = ne.size.begin(), *nd = ne.data.begin();
        const int *nls = nodeloc ? nodeloc->start.begin() : nullptr;
        int *nld = nodeloc ? nodeloc->data.begin() : nullptr;
        const int *els = elementloc ? elementloc->start.begin() : nullptr;
        int *eld = elementloc ? elementloc->data.begin() : nullptr;
        const bool jn = ne.isjagged(), je = en.isjagged();
        constexpr int CHUNK = 4096;
        const int nchunks = (nn + CHUNK - 1) / CHUNK;
        int bad = 0;
#pragma omp parallel for schedule(dynamic, 1) reduction(+ : bad)
        for (int c = 0; c < nchunks; ++c)
        {
            const int n0 = c * CHUNK, n1 = std::min(nn, n0 + CHUNK);
            if (!jn && !je)
                bad += locate<false, false>(n0, n1, es, ez, ed, ns, nz, nd, nls, nld, els, eld);
            else if (jn && !je)
                bad += locate<true, false>(n0, n1, es, ez, ed, ns, nz, nd, nls, nld, els, eld);
            else if (!jn && je)
                bad += locate<false, true>(n0, n1, es, ez, ed, ns, nz, nd, nls, nld, els, eld);
            else
                bad += locate<true, true>(n0, n1, es, ez, ed, ns, nz, nd, nls, nld, els, eld);
        }
        if (bad)
            throw std::runtime_error("getlocations: the two relations are not transposes of each other");
    }
} // namespace

void hidden::getlocations(o2m const &ne, o2m const &en, o2m &nodeloc, o2m &elementloc)
{
    locations(ne, en, &nodeloc, &elementloc);
}

o2m hidden::getnodepositions(o2m const &ne, o2m const &en)
{
    o2m nodeloc;
    locations(ne, en, &nodeloc, nullptr);
    return nodeloc;
}

o2m hidden::getelementpositions(o2m const &ne, o2m const &en)
{
    o2m elementloc;
    locations(ne, en, nullptr, &elementloc);
    return elementloc;
}

o2m getcliques(o2m const &ne, o2m const &en)
{
    o2m cliques;
    const int nel = ne.nelem;
    setsize(cliques, nel);
    {
        std::unique_ptr<int[]> sizes(new int[static_cast<std::size_t>(std::max(nel, 1))]);
        for (int e = 0; e < nel; ++e)
        {
            const int ns = ne.nnodes(e);
            sizes[static_cast<std::size_t>(e)] = ns * ns;
        }
        const long long nnz = exclusivescan(sizes.get(), nel, cliques.start.begin());
        setsizeexact(cliques.data, nnz);
    }
    o2m nodeloc = hidden::getnodepositions(ne, en);
    const int nn = en.nelem;
#pragma omp parallel
    {
        minihash slots(256);
#pragma omp for schedule(dynamic, 1024)
        for (int node1 = 0; node1 < nn; ++node1)
        {
            slots.reset();
            int nnz1 = 0;
            crowview els = en[node1];
            crowview loc = nodeloc[node1];
            for (int k = 0; k < els.n; ++k)
            {
                const int lnode1 = loc.p[k];
                const int element = els.p[k];
                crowview nodes = ne[element];
                const int esize = nodes.n;
                rowview cl = cliques[element];
                for (int lnode2 = 0; lnode2 < esize; ++lnode2)
                {
                    bool inserted;
                    const int v = slots.findorinsert(nodes.p[lnode2], nnz1, inserted);
                    if (inserted)
                        ++nnz1;
                    cl.p[lnode2 + lnode1 * esize] = v;
                }
            }
        }
    }
    cliques.maxnode = 0;
    return cliques;
}
