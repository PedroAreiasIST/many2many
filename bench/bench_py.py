#!/usr/bin/env python3
"""Baseline for the IJNME revision: the three relational operations of the paper
(symbolic transpose, node-to-node composition A^T.A, element lookup by node list)
executed with SciPy sparse and with SuiteSparse:GraphBLAS (python-graphblas) on the
same structured H8 meshes used by the library benchmarks.

Usage: bench_py.py <backend: scipy|graphblas> <nee> [reps] [nthreads]
Prints one JSON line per measurement.
"""
import json
import sys
import time

import numpy as np


def h8_connectivity(nee):
    s1 = nee + 1
    s2 = s1 * s1
    ix, iy, iz = np.meshgrid(np.arange(nee), np.arange(nee), np.arange(nee), indexing="ij")
    base = (ix + iy * s1 + iz * s2).ravel().astype(np.int64)
    off = np.array([0, 1, 1 + s1, s1, s2, 1 + s2, 1 + s1 + s2, s1 + s2], dtype=np.int64)
    conn = base[:, None] + off[None, :]
    return conn  # (nel, 8), node ids 0-based


def median(f, reps):
    ts = []
    out = None
    for _ in range(reps):
        t0 = time.perf_counter()
        out = f()
        ts.append(time.perf_counter() - t0)
    return float(np.median(ts)), out


def run_scipy(nee, reps):
    import scipy.sparse as sp

    conn = h8_connectivity(nee)
    nel = conn.shape[0]
    nn = (nee + 1) ** 3
    indptr = np.arange(0, 8 * nel + 1, 8, dtype=np.int32)
    indices = conn.ravel().astype(np.int32)
    data = np.ones(indices.shape[0], dtype=np.int8)
    A = sp.csr_matrix((data, indices, indptr), shape=(nel, nn))
    del conn

    # 1. symbolic transpose (node -> element), materialised as CSR
    t_tr, AT = median(lambda: A.transpose().tocsr(), reps)
    # 2. node-to-node composition (assembly sparsity pattern)
    t_mul, P = median(lambda: AT @ A, reps)
    nnz = int(P.nnz)
    del P
    # 3. lookup of an element from its node list: intersection of the transpose rows
    rng = np.random.default_rng(1)
    q = rng.integers(0, nel, size=1000)
    ATi, ATp = AT.indices, AT.indptr

    def lookup(e):
        nodes = A.indices[A.indptr[e]:A.indptr[e + 1]]
        res = ATi[ATp[nodes[0]]:ATp[nodes[0] + 1]]
        for n in nodes[1:]:
            res = np.intersect1d(res, ATi[ATp[n]:ATp[n + 1]], assume_unique=True)
        return res

    t0 = time.perf_counter()
    found = 0
    for e in q:
        r = lookup(int(e))
        found += int(r.size == 1 and r[0] == e)
    t_lookup = (time.perf_counter() - t0) / len(q)
    assert found == len(q), found
    return dict(backend="scipy", threads=1, nee=nee, nel=nel, nn=nn, t_transpose=t_tr,
                t_product=t_mul, nnz_product=nnz, t_lookup_us=1e6 * t_lookup)


def run_graphblas(nee, reps, nthreads):
    import graphblas as gb

    gb.ss.config["nthreads"] = nthreads
    conn = h8_connectivity(nee)
    nel = conn.shape[0]
    nn = (nee + 1) ** 3
    rows = np.repeat(np.arange(nel, dtype=np.int64), 8)
    cols = conn.ravel()
    del conn
    A = gb.Matrix.from_coo(rows, cols, True, nrows=nel, ncols=nn, dtype=gb.dtypes.BOOL)
    del rows, cols
    A.ss.config["format"] = "by_row"

    def transpose():
        AT = A.T.new()
        AT.ss.config["format"] = "by_row"
        AT.wait()
        return AT

    t_tr, AT = median(transpose, reps)

    def product():
        P = AT.mxm(A, gb.semiring.any_pair[bool]).new()
        P.wait()
        return P

    t_mul, P = median(product, reps)
    nnz = int(P.nvals)
    del P
    rng = np.random.default_rng(1)
    q = rng.integers(0, nel, size=1000)

    def lookup(e):
        nodes = A[e, :].new().to_coo()[0]
        v = AT[int(nodes[0]), :].new()
        for n in nodes[1:]:
            v << v.ewise_mult(AT[int(n), :].new(), gb.binary.pair)
        return v.to_coo()[0]

    t0 = time.perf_counter()
    found = 0
    for e in q:
        r = lookup(int(e))
        found += int(r.size == 1 and r[0] == e)
    t_lookup = (time.perf_counter() - t0) / len(q)
    assert found == len(q), found
    return dict(backend="graphblas", threads=nthreads, nee=nee, nel=nel, nn=nn,
                t_transpose=t_tr, t_product=t_mul, nnz_product=nnz,
                t_lookup_us=1e6 * t_lookup, version=gb.__version__)


if __name__ == "__main__":
    backend = sys.argv[1]
    nee = int(sys.argv[2])
    reps = int(sys.argv[3]) if len(sys.argv) > 3 else 3
    nthreads = int(sys.argv[4]) if len(sys.argv) > 4 else 16
    if backend == "scipy":
        r = run_scipy(nee, reps)
    else:
        r = run_graphblas(nee, reps, nthreads)
    print(json.dumps(r), flush=True)
