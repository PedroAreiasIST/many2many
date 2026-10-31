#ifndef RELATIONONETOMANY_HPP
#define RELATIONONETOMANY_HPP
#include "basics.hpp"
#include "seque.hpp"
#include <cstring>
#include <functional>
#include <initializer_list>
#include <iostream>

/**
 * @brief One-to-many relation (one connectivity table) in compressed row storage.
 *
 * All node lists live in one pooled array (\c data). Row e occupies
 * [start[e], start[e+1]) when the relation is packed (the CSR form of the
 * sparse-matrix literature: one 32-bit offset per row); start has nelem+1
 * entries. Rows that are emptied, shortened or regrown individually turn the
 * relation jagged: a per-row \c size array is materialized, a grown row is
 * relocated to the end of the pool, and \c pack() restores the contiguous form.
 * Every bulk kernel (transpose, composition, set algebra, compression,
 * local positions, cliques) produces packed output. Rows are read and written
 * through zero-copy views (\c rowview, \c crowview); a view converts to a
 * \c seque<int> when a copy is wanted.
 */
struct rowview;

/** Read-only view of one row: a pointer and a length, no ownership. */
struct crowview
{
    const int *p{nullptr};
    int n{0};
    crowview() = default;
    crowview(const int *ptr, int len) noexcept : p(ptr), n(len) {}
    crowview(seque<int> const &s) noexcept : p(s.begin()), n(s.size) {}
    crowview(std::initializer_list<int> il) noexcept : p(il.begin()), n(static_cast<int>(il.size())) {}
    int size() const noexcept { return n; }
    int operator[](int i) const noexcept { return p[i]; }
    const int *begin() const noexcept { return p; }
    const int *end() const noexcept { return p + n; }
    operator seque<int>() const
    {
        seque<int> s(n);
        if (n > 0)
            std::memcpy(s.begin(), p, static_cast<size_t>(n) * sizeof(int));
        return s;
    }
};

/** Mutable view of one row (same size; entries may be rewritten in place). */
struct rowview
{
    int *p{nullptr};
    int n{0};
    rowview() = default;
    rowview(int *ptr, int len) noexcept : p(ptr), n(len) {}
    int size() const noexcept { return n; }
    int &operator[](int i) const noexcept { return p[i]; }
    int *begin() const noexcept { return p; }
    int *end() const noexcept { return p + n; }
    operator crowview() const noexcept { return crowview(p, n); }
    operator seque<int>() const
    {
        seque<int> s(n);
        if (n > 0)
            std::memcpy(s.begin(), p, static_cast<size_t>(n) * sizeof(int));
        return s;
    }
};

inline int getsize(crowview r) noexcept { return r.n; }
inline int getsize(rowview r) noexcept { return r.n; }

inline bool operator==(crowview a, crowview b) noexcept
{
    return a.n == b.n && (a.n == 0 || std::memcmp(a.p, b.p, static_cast<size_t>(a.n) * sizeof(int)) == 0);
}

inline bool operator!=(crowview a, crowview b) noexcept { return !(a == b); }

/** Lexicographic comparison of two rows (a shorter prefix is smaller). */
inline bool lexicompare(crowview a, crowview b) noexcept
{
    const int m = a.n < b.n ? a.n : b.n;
    for (int i = 0; i < m; ++i)
    {
        if (a.p[i] != b.p[i])
            return a.p[i] < b.p[i];
    }
    return a.n < b.n;
}

inline std::ostream &operator<<(std::ostream &os, crowview r)
{
    os << r.n << ' ';
    for (int i = 0; i < r.n; ++i)
        os << r.p[i] << ' ';
    return os;
}

inline std::ostream &operator<<(std::ostream &os, rowview r) { return os << crowview(r); }

struct o2m
{
    /** Row starts: nelem+1 entries, start[0] = 0. */
    seque<int> start{0};
    /** Row sizes; empty while the rows are contiguous (packed), nelem entries otherwise. */
    seque<int> size;
    /** Pooled node indices of all rows. */
    seque<int> data;
    int nelem{0};
    /** Highest node index seen across all elements. */
    int maxnode{0};

    bool isjagged() const noexcept { return size.size != 0; }

    rowview operator[](int element) noexcept
    {
        const int s = start[element];
        const int n = size.size ? size[element] : start[element + 1] - s;
        return rowview(data.begin() + s, n);
    }

    crowview operator[](int element) const noexcept
    {
        const int s = start[element];
        const int n = size.size ? size[element] : start[element + 1] - s;
        return crowview(data.begin() + s, n);
    }

    int nelems() const noexcept { return nelem; }

    int nnodes(int element) const noexcept
    {
        return size.size ? size[element] : start[element + 1] - start[element];
    }
};

bool operator==(o2m const &a, o2m const &b);
inline bool operator!=(o2m const &a, o2m const &b) { return !(a == b); }
bool operator<(o2m const &a, o2m const &b);
inline bool operator>(o2m const &a, o2m const &b) { return b < a; }
inline bool operator<=(o2m const &a, o2m const &b) { return !(b < a); }
inline bool operator>=(o2m const &a, o2m const &b) { return !(a < b); }
std::ostream &operator<<(std::ostream &os, o2m const &rel);
std::istream &operator>>(std::istream &is, o2m &rel);
std::size_t hash_value(o2m const &rel);

template<>
struct std::hash<o2m>
{
    std::size_t operator()(o2m const &rel) const noexcept { return hash_value(rel); }
};

/** Sets the number of rows; new rows are empty. */
void setsize(o2m &rel, int nelem);

/** Reserves pool and header capacity ahead of a sequence of appends. */
void reserve(o2m &rel, int nelem, long long nentries);

/** Appends a row; throws if the node list has repeated entries. @return its index. */
int appendelement(o2m &rel, crowview nodes);

/** Replaces row \p element (in place when it does not grow, else relocated to the pool end). */
void setrow(o2m &rel, int element, crowview nodes);

/** Empties row \p element. */
void clearrow(o2m &rel, int element);

/** Appends one node to row \p element (in place when the row ends the pool, else relocated). */
void appendtorow(o2m &rel, int element, int node);

/** Appends an element from an initializer list of node ids. */
o2m &operator<<(o2m &rel, std::initializer_list<int> nodes);

/** Number of stored entries (sum of row sizes). */
long long getnnz(o2m const &rel);

/** True when the rows are contiguous and in order (CSR form). */
bool ispacked(o2m const &rel);

/** Rewrites the pool in CSR form, dropping the space of relocated rows. */
void pack(o2m &rel);

/** Index maps of the unique rows given a lexicographic order (see indicesfromorder of seque):
 *  newfromold[i] = index of the first row equal to row i among the unique rows (in order of first appearance),
 *  oldfromnew[u] = row index of unique row u. */
void indicesfromorder(o2m const &rel, seque<int> const &order, seque<int> &newfromold, seque<int> &oldfromnew);

/** Recomputes maxnode from the stored rows. */
void updatemaxnode(o2m &rel);

/** Copy of the rows listed in \p order, in that order (packed). */
o2m getrows(o2m const &rel, seque<int> const &order);

/** Returns indices of elements whose node lists appear earlier in the relation. */
seque<int> getduplicates(o2m const &rel);

/** Given a lexicographic order of the rows, the indices of rows equal to a preceding one. */
seque<int> getindicesofduplicates(o2m const &rel, seque<int> const &order);

/** Transpose: node-to-element relation from an element-to-node relation (rows sorted). */
o2m Tr(o2m const &rel);

/** Symbolic composition (sparse product without values) of two relations. */
o2m operator*(o2m const &rela, o2m const &relb);

/** Composition with the identity relation of a sequence. */
o2m operator*(o2m const &rela, seque<int> const &vec);

/** Row-wise union (entries of \p rela first, then the new entries of \p relb). */
o2m operator+(o2m const &rela, o2m const &relb);

/** Alias for operator+. */
o2m operator||(o2m const &a, o2m const &b);

/** Row-wise intersection (result rows sorted). */
o2m operator&&(o2m const &a, o2m const &b);

/** Row-wise set difference rela \ relb (result rows sorted). */
o2m operator-(o2m const &rela, o2m const &relb);

/** Topological order of the elements of a directed acyclic relation. */
seque<int> gettoporder(o2m const &rel);

/** Lexicographic ordering permutation of the rows. */
seque<int> getorder(o2m const &rel);

/** Identity relation of a sequence: element i -> {i}. */
o2m geto2mfromsequence(seque<int> const &other);

/** Clique addressing: for each element the ns*ns slot numbers of its node pairs. */
o2m getcliques(o2m const &nodesfromelement, o2m const &elementsfromnode);

/** Set operations on sorted rows, returning a new list. */
seque<int> getintersection(crowview a, crowview b);
seque<int> getunion(crowview a, crowview b);
seque<int> getdifference(crowview a, crowview b);

namespace hidden
{
    /** Keeps the rows listed in oldelementfromnew (packed), updates nelem/maxnode. */
    void compresselements(o2m &rel, seque<int> const &oldelementfromnew);

    /** Replaces node indices using the newnodefromold map. */
    void permutenodes(o2m &rel, seque<int> const &newnodefromold);

    /** Local positions: nodeloc[node][k] = position of node in element en[node][k];
     *  elementloc[element][i] = position of element in the list of its node ne[element][i]. */
    void getlocations(o2m const &nodesfromelement, o2m const &elementsfromnode,
                      o2m &nodeloc, o2m &elementloc);

    /** nodeloc only (see getlocations). */
    o2m getnodepositions(o2m const &nodesfromelement, o2m const &elementsfromnode);

    /** elementloc only (see getlocations). */
    o2m getelementpositions(o2m const &nodesfromelement, o2m const &elementsfromnode);

    /** A relation with the row sizes of \p shape and uninitialized entries. */
    o2m getshape(o2m const &shape);
} // namespace hidden

#endif
