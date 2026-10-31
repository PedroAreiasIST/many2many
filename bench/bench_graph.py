#!/usr/bin/env python3
"""SciPy side of the derived-query comparison: node-to-node graph of an H8 mesh (A^T A),
reverse Cuthill-McKee ordering and bandwidth, connected components, breadth-first levels.
Usage: bench_graph.py <nee> [reps]  -> one JSON line (medians of reps)."""
import json
import sys
import time

import numpy as np
import scipy
import scipy.sparse as sp
import scipy.sparse.csgraph as cg


def h8(nee):
    s1 = nee + 1
    s2 = s1 * s1
    iex, iey, iez = np.meshgrid(np.arange(nee), np.arange(nee), np.arange(nee), indexing="ij")
    base = (iex + iey * s1 + iez * s2).ravel()
    conn = np.stack([base, base + 1, base + 1 + s1, base + s1, base + s2, base + 1 + s2, base + 1 + s1 + s2, base + s1 + s2], 1)
    return conn.astype(np.int32), s1 * s1 * s1


def median(v):
    return sorted(v)[len(v) // 2]


def bandwidth(graph, perm=None):
    coo = graph.tocoo()
    r, c = coo.row, coo.col
    if perm is not None:
        inv = np.empty_like(perm)
        inv[perm] = np.arange(len(perm))
        r, c = inv[r], inv[c]
    return int(np.max(np.abs(r.astype(np.int64) - c.astype(np.int64))))


nee = int(sys.argv[1]) if len(sys.argv) > 1 else 100
reps = int(sys.argv[2]) if len(sys.argv) > 2 else 3
conn, nn = h8(nee)
nel, k = conn.shape
# deterministic shuffle of the node numbering (same rule as bench_graph.cpp)
keys = (np.arange(nn, dtype=np.uint64) * np.uint64(2654435761)) & np.uint64(0xFFFFFFFF)
order = np.lexsort((np.arange(nn), keys))
newfromold = np.empty(nn, np.int32)
newfromold[order] = np.arange(nn, dtype=np.int32)
A = sp.csr_matrix((np.ones(nel * k, np.int8), conn.ravel(), np.arange(0, nel * k + 1, k)), shape=(nel, nn))
AT = A.transpose().tocsr()
P = (AT @ A).tocsr()  # node-to-node graph (symmetric, with diagonal)
P.data[:] = 1
connp = newfromold[conn]
Ap = sp.csr_matrix((np.ones(nel * k, np.int8), connp.ravel(), np.arange(0, nel * k + 1, k)), shape=(nel, nn))
Pp = (Ap.transpose().tocsr() @ Ap).tocsr()
Pp.data[:] = 1
t_rcm, t_cc, t_bfs, t_ccel, t_dual = [], [], [], [], []
for _ in range(reps):
    t0 = time.perf_counter()
    perm = cg.reverse_cuthill_mckee(Pp, symmetric_mode=True)
    t_rcm.append(time.perf_counter() - t0)
    t0 = time.perf_counter()
    D = (A @ AT).tocsr()                 # element-to-element with multiplicities
    D.data[D.data < 4] = 0
    D.eliminate_zeros()
    D.setdiag(0)
    D.eliminate_zeros()
    t_dual.append(time.perf_counter() - t0)
    t0 = time.perf_counter()
    ncomp, labels = cg.connected_components(P, directed=False)
    t_cc.append(time.perf_counter() - t0)
    t0 = time.perf_counter()
    order, pred = cg.breadth_first_order(P, 0, directed=False, return_predecessors=True)
    t_bfs.append(time.perf_counter() - t0)
    # components of the elements through shared nodes: bipartite graph [[0, A],[A^T, 0]]
    B = sp.bmat([[None, A], [AT, None]], format="csr")
    t0 = time.perf_counter()
    ncel, lab = cg.connected_components(B, directed=False)
    t_ccel.append(time.perf_counter() - t0)
bw0 = bandwidth(P)
bwp = bandwidth(Pp)
bw1 = bandwidth(Pp, perm)
print(json.dumps({"backend": "scipy", "nee": nee, "nel": int(nel), "nn": int(nn), "nnz_graph": int(P.nnz),
                  "t_rcm": median(t_rcm), "bandwidth_natural": bw0, "bandwidth_shuffled": bwp, "bandwidth_rcm": bw1,
                  "t_dual_faces": median(t_dual), "nnz_dual_faces": int(D.nnz),
                  "t_components_nodes": median(t_cc), "ncomponents": int(ncomp),
                  "t_components_elements": median(t_ccel), "ncomponents_elements": int(ncel),
                  "t_bfs": median(t_bfs), "scipy": scipy.__version__}))
