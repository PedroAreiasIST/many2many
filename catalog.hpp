#ifndef M2M_CATALOGUE_HPP
#define M2M_CATALOGUE_HPP
// Catalog of the standard elements: symmetry groups (the rotations that keep the element's
// orientation) and builders of their faces and edges, as local node positions. Declared on a
// zoo with declare<E, N, F, G>(garden, catalog::tet4()) and the like.
#include "seque.hpp"
#include <type_traits>

namespace catalog
{
    /** One element type of the catalog. */
    struct element
    {
        seque<seque<int> > group;   ///< symmetry group (permutations of the local positions)
        seque<seque<int> > tris;    ///< local nodes of each triangular face
        seque<seque<int> > quads;   ///< local nodes of each quadrilateral face
        seque<seque<int> > edges;   ///< local nodes of each edge
    };

    /** The cyclic group of order n (rotations of an oriented polygon). */
    inline seque<seque<int> > cyclic(int n)
    {
        seque<seque<int> > g;
        for (int r = 0; r < n; ++r)
        {
            seque<int> p(n);
            for (int k = 0; k < n; ++k)
                p[k] = (k + r) % n;
            append(g, p);
        }
        return g;
    }

    /** The dihedral group of order 2n (rotations and reflections: a polygon seen from either
     *  side), the group of a face shared by two volumes. */
    inline seque<seque<int> > dihedral(int n)
    {
        seque<seque<int> > g = cyclic(n);
        for (int r = 0; r < n; ++r)
        {
            seque<int> p(n);
            for (int k = 0; k < n; ++k)
                p[k] = ((n - k) + r) % n;
            append(g, p);
        }
        return g;
    }

    inline element bar2()
    {
        return {{{0, 1}, {1, 0}}, {}, {}, {}};
    }

    inline element tri3()
    {
        return {{{0, 1, 2}, {1, 2, 0}, {2, 0, 1}}, {}, {}, {{0, 1}, {1, 2}, {2, 0}}};
    }

    inline element quad4()
    {
        return {{{0, 1, 2, 3}, {1, 2, 3, 0}, {2, 3, 0, 1}, {3, 0, 1, 2}}, {}, {}, {{0, 1}, {1, 2}, {2, 3}, {3, 0}}};
    }

    inline element tet4()
    {
        return {{{0, 1, 2, 3}, {1, 2, 0, 3}, {2, 0, 1, 3}, {0, 3, 1, 2}, {3, 1, 0, 2}, {0, 2, 3, 1},
                 {0, 3, 2, 1}, {3, 0, 1, 2}, {2, 1, 3, 0}, {1, 3, 0, 2}, {3, 2, 0, 1}, {2, 0, 3, 1}},
                {{0, 1, 3}, {0, 3, 2}, {1, 0, 2}, {1, 2, 3}},
                {},
                {{0, 1}, {1, 2}, {2, 0}, {0, 3}, {1, 3}, {2, 3}}};
    }

    inline element wedge6()
    {
        return {{{0, 1, 2, 3, 4, 5}, {1, 2, 0, 4, 5, 3}, {2, 0, 1, 5, 3, 4}, {0, 2, 1, 3, 5, 4}, {2, 1, 0, 5, 4, 3},
                 {1, 0, 2, 4, 3, 5}},
                {{0, 1, 2}, {4, 3, 5}},
                {{1, 4, 5, 2}, {1, 0, 3, 4}, {0, 2, 5, 3}},
                {{0, 1}, {1, 2}, {2, 0}, {3, 4}, {4, 5}, {5, 3}, {0, 3}, {1, 4}, {2, 5}}};
    }

    inline element hex8()
    {
        return {{{0, 1, 2, 3, 4, 5, 6, 7}, {1, 2, 3, 0, 5, 6, 7, 4}, {2, 3, 0, 1, 6, 7, 4, 5}, {3, 0, 1, 2, 7, 4, 5, 6},
                 {4, 0, 3, 7, 5, 1, 2, 6}, {5, 4, 7, 6, 1, 0, 3, 2}, {1, 5, 6, 2, 0, 4, 7, 3}, {2, 6, 7, 3, 1, 5, 4, 0},
                 {3, 2, 6, 7, 0, 1, 5, 4}, {7, 6, 5, 4, 3, 2, 1, 0}, {4, 5, 1, 0, 7, 6, 2, 3}, {0, 3, 7, 4, 1, 2, 6, 5},
                 {0, 4, 5, 1, 3, 7, 6, 2}, {5, 1, 0, 4, 6, 2, 3, 7}, {1, 0, 4, 5, 2, 3, 7, 6}, {2, 1, 5, 6, 3, 0, 4, 7},
                 {6, 5, 4, 7, 2, 1, 0, 3}, {7, 4, 0, 3, 6, 5, 1, 2}, {3, 7, 6, 2, 0, 4, 5, 1}, {1, 0, 3, 2, 5, 4, 7, 6},
                 {2, 1, 0, 3, 6, 5, 4, 7}, {3, 2, 1, 0, 7, 6, 5, 4}, {0, 3, 2, 1, 4, 7, 6, 5}, {6, 7, 3, 2, 5, 4, 0, 1}},
                {},
                {{0, 1, 5, 4}, {1, 2, 6, 5}, {2, 3, 7, 6}, {3, 0, 4, 7}, {4, 5, 6, 7}, {1, 0, 3, 2}},
                {{0, 1}, {1, 2}, {2, 3}, {3, 0}, {4, 5}, {5, 6}, {6, 7}, {7, 4}, {0, 4}, {1, 5}, {2, 6}, {3, 7}}};
    }
} // namespace catalog

/** Declares on \p garden the symmetry group of E over N and, for the child types given (void to
 *  skip), the builders of its triangular faces (T), quadrilateral faces (Q) and edges (G). The
 *  groups of the catalog preserve orientation (rotations); a face type that is shared between
 *  volumes must be declared with the dihedral group, definesymmetrygroup<F, N>(dihedral(k)). */
template<typename E, typename N, typename T = void, typename Q = void, typename G = void, typename Z>
void declare(Z &garden, catalog::element const &def)
{
    garden.template definesymmetrygroup<E, N>(def.group);
    if constexpr (!std::is_void_v<T>)
        for (int i = 0; i < getsize(def.tris); ++i)
            garden.template definechildrenbuilder<E, T, N>(def.tris[i]);
    if constexpr (!std::is_void_v<Q>)
        for (int i = 0; i < getsize(def.quads); ++i)
            garden.template definechildrenbuilder<E, Q, N>(def.quads[i]);
    if constexpr (!std::is_void_v<G>)
        for (int i = 0; i < getsize(def.edges); ++i)
            garden.template definechildrenbuilder<E, G, N>(def.edges[i]);
}

#endif
