// getorder and indicesfromorder against the reference definitions (stable lexicographic sort,
// sequential run numbering) on random relations large enough for the parallel radix paths:
// rows of at most 2, 4 and 8 entries, with repeated rows and runs of equal prefixes.
#include "o2m.hpp"
#include <algorithm>
#include <cstdio>
#include <numeric>
#include <random>

static int fails = 0;
#define CHECK(c, msg) do { if (!(c)) { ++fails; std::printf("FAIL: %s\n", msg); } } while (0)

static void testcase(int n, int maxlen, int maxnode, bool fixedlen, unsigned seed)
{
    std::mt19937 rng(seed);
    o2m rel;
    for (int i = 0; i < n; ++i)
    {
        const int len = fixedlen ? maxlen : 1 + static_cast<int>(rng() % static_cast<unsigned>(maxlen));
        seque<int> row(len);
        for (int k = 0; k < len; ++k)
            row[k] = static_cast<int>(rng() % static_cast<unsigned>(maxnode + 1));
        if (i % 7 == 3 && i > 0) // repeated rows
            row = rel[i - 1];
        if (i % 5 == 1 && i > 0 && len >= 2) // equal prefix, different tail
        {
            crowview p = rel[i - 1];
            row[0] = p.p[0];
            if (p.n > 1)
                row[1] = p.p[1];
        }
        // appendelement rejects repeated nodes: make the row a set
        std::sort(row.begin(), row.end());
        seque<int> uniq;
        for (int k = 0; k < getsize(row); ++k)
            if (k == 0 || row[k] != row[k - 1])
                append(uniq, row[k]);
        std::shuffle(uniq.begin(), uniq.end(), rng);
        appendelement(rel, uniq);
    }
    // reference order
    seque<int> ref(n);
    std::iota(ref.begin(), ref.end(), 0);
    std::stable_sort(ref.begin(), ref.end(), [&rel](int i, int j) { return lexicompare(rel[i], rel[j]); });
    seque<int> ord = getorder(rel);
    bool same = ord.size == n;
    for (int i = 0; same && i < n; ++i)
        same = ord[i] == ref[i];
    char msg[128];
    std::snprintf(msg, sizeof msg, "getorder n=%d maxlen=%d maxnode=%d", n, maxlen, maxnode);
    CHECK(same, msg);
    // reference index maps
    seque<int> nfo(n), ofn;
    int u = -1;
    for (int i = 0; i < n; ++i)
    {
        if (i == 0 || rel[ref[i]] != rel[ref[i - 1]])
        {
            ++u;
            append(ofn, ref[i]);
        }
        nfo[ref[i]] = u;
    }
    seque<int> nfo2, ofn2;
    indicesfromorder(rel, ord, nfo2, ofn2);
    bool same2 = nfo2.size == n && ofn2.size == ofn.size;
    for (int i = 0; same2 && i < n; ++i)
        same2 = nfo2[i] == nfo[i];
    for (int i = 0; same2 && i < ofn.size; ++i)
        same2 = ofn2[i] == ofn[i];
    std::snprintf(msg, sizeof msg, "indicesfromorder n=%d maxlen=%d maxnode=%d (%d unique)", n, maxlen, maxnode, ofn.size);
    CHECK(same2, msg);
    std::printf("n=%d maxlen=%d maxnode=%d: %d unique rows, %s\n", n, maxlen, maxnode, ofn.size, same && same2 ? "ok" : "MISMATCH");
}

int main()
{
    testcase(300000, 2, 2000, false, 1);     // edges, small node range: many repeats
    testcase(300000, 2, 2000000, true, 2);   // edges, wide range
    testcase(300000, 4, 3000, false, 3);     // faces (mixed lengths)
    testcase(300000, 4, 5000000, true, 4);
    testcase(300000, 8, 4000, false, 5);     // long rows, runs of equal prefix
    testcase(300000, 8, 1000000, true, 6);
    testcase(20000, 8, 100, false, 7);       // below the parallel thresholds (comparison paths)
    testcase(20000, 3, 50, false, 8);
    if (fails == 0)
        std::printf("all order checks passed\n");
    return fails;
}
