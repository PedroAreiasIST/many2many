![](m2m.png)

# many2many

A C++20 library for the many-to-many relations of discretization software: element-node,
face-node and edge-node connectivities, boundary-condition sets, element groups and the
dependencies between them. One table type in compressed row storage, three symbolic
operations (transpose, composition, row-wise set algebra) and two schema-level rules
(canonical node lists under a symmetry group, synthesized children that record their
parents) generate every derived adjacency. The paper describing the framework and this
implementation is "A Software Library for Type-Safe Management of Many-to-Many Relations
and Dependencies in Computational Mechanics" (Areias, Rabczuk, Vaz; IJNME, revised 2026).

## Containers

| File | Container |
|---|---|
| `seque.hpp` | small-buffer-optimized sequence for short lists (node lists, query results) |
| `o2m.hpp/.cpp` | one-to-many relation in compressed row storage with zero-copy row views; transpose, composition, set algebra, ordering, index maps, compression, local positions, cliques |
| `m2m.hpp/.cpp` | a relation and its transpose kept synchronized; lookup by node list; neighbours |
| `mm2m.hpp/.cpp` | the matrix of relations over types; cascade deletion |
| `zoo.hpp`, `thing.hpp/.cpp`, `symmetries.hpp` | typed layer: symmetry groups, builders, synthesis with parent tracking (per element or in bulk: `synthesize<E,C,N>()`), deduplication, boundary (`getboundary<C,E>()`), reference-counted removal |
| `graph.hpp/.cpp` | derived queries: dual graphs by shared count (single-pass counting composition), node graph, components (parallel union-find), breadth-first levels (parallel, level-synchronous), k-hop neighbourhoods, reverse Cuthill-McKee (parallel pseudo-peripheral search, prefetching numbering), bandwidth, greedy colouring, synchronized append |
| `catalogue.hpp` | the standard elements (bar2, tri3, quad4, tet4, wedge6, hex8): rotation groups, faces and edges as builders; cyclic and dihedral groups |
| `typseque.hpp`, `superstruct.hpp` | compile-time type lists and the heterogeneous data container |
| `remesh*.cpp/.hpp`, `meshquality.*` | the simplex mesh splitter ported from Fortran (SimPlas) |

The bulk kernels are out-of-line functions on `__restrict` CSR arrays called in chunks from
OpenMP loops; the Intel compiler (icpx) produces the fastest code, g++ is within 12% at 16
threads. The lexicographic order of rows (`getorder`) is a parallel radix sort of packed keys
(rows of up to four entries) or of the first two entries followed by the comparison of the runs
(longer rows); `indicesfromorder` is parallel; `appendelements` appends a whole relation at
once. A relation holds at most 2^31-1 incidences.

## Build

```
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release && cmake --build build   # library targets
cd bench && make                     # drivers (g++)
cd bench && ./build_icpx.sh bench_lib bench_tables bench_graph test_order   # the same with icpx, as <target>_icpx
```
`bench/Makefile` needs the cereal headers (`CEREAL_INC`, by default the FetchContent copy
under `build/_deps/cereal-src/include` after the CMake configure) and TBB (`-ltbb`).

## Benchmarks (bench/)

* `running_example.cpp`: the running example of the paper (7 nodes, a tetrahedron, two edges,
  a wedge), printing every relation after insertion, deduplication and removal.
* `bench_lib.cpp`, `bench_f90.f90`, `bench_py.py`, `run_chain.sh`: the same-machine comparison
  of transpose, composition and lookup with SciPy, SuiteSparse:GraphBLAS (python-graphblas)
  and the SimPlas Fortran routines; `results.jsonl` holds the raw results.
* `bench_tables.cpp`, `run_tables.sh`: the scaling tables of the paper (`results_tables.jsonl`).
* `bench_graph.cpp`, `bench_graph.py`, `run_graph.sh`: derived queries against SciPy csgraph and the
  bulk-synthesis throughput (`results_graph.jsonl`); `test_graph.cpp`, `test_synth.cpp`: their checks;
  `test_order.cpp`: `getorder` and `indicesfromorder` against their reference definitions on random
  relations (every radix path).
* `run_all.sh`: every measurement in sequence; `run_after_kernels.sh`: the subset re-measured after the
  ordering and graph kernels changed (splitter, order table, derived queries).
* `run_remesh.sh`: the splitter against the Fortran original (`remesh_timings.txt`).

Measurements in the paper: 16 OpenMP threads pinned to the eight performance cores of an
Intel i9-13900HX (`OMP_PROC_BIND=close OMP_PLACES=cores taskset -c 0-15`) and one thread;
medians of five runs. Environment variables for experiments: `M2M_TR_MODE` (1 histogram
fill, 2 atomic fill and sort, 3 staged fill), `M2M_MUL_MODE` (1 stamp, 2 byte, 3 bit marker),
`M2M_MUL_PASS` (1 single pass, 2 two passes), `M2M_MUL_BUDGET_GB`, `M2M_TIMING`.

Written by Pedro Areias (IST). GPL-3.0.
