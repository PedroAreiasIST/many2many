#!/bin/bash
# Same-machine comparison, one process at a time, every code pinned to the eight performance
# cores (CPUs 0-15) with 16 OpenMP threads and with 1 thread; medians of REPS runs.
# Backends: Fortran (SimPlas routines), this library (icpx and g++ builds), SuiteSparse:GraphBLAS
# (python-graphblas), SciPy (single-threaded). Results: one JSON line per measurement.
cd "$(dirname "$0")"
source /opt/intel/oneapi/setvars.sh >/dev/null 2>&1
REPS=${REPS:-5}
OUT=${OUT:-results.jsonl}; : > $OUT
PIN="taskset -c 0-15"
export OMP_PROC_BIND=close OMP_PLACES=cores
PY=/tmp/claude-1000/-home-pedro-Desktop-NovosPapers-remesh-tosendijnme/1f833e97-2ab4-4f0c-81f4-5620added823/scratchpad/venv/bin/python
F90=/tmp/claude-1000/-home-pedro-Desktop-NovosPapers-remesh-tosendijnme/1f833e97-2ab4-4f0c-81f4-5620added823/scratchpad/bench/bench_f90
for nee in 100 200 300; do
  for th in 16 1; do
    OMP_NUM_THREADS=$th $PIN $F90 $nee $REPS >> $OUT 2>>chain.err
    OMP_NUM_THREADS=$th $PIN ./bench_lib_icpx $nee $REPS | sed 's/"library"/"library-icpx"/' >> $OUT 2>>chain.err
    OMP_NUM_THREADS=$th $PIN ./bench_lib $nee $REPS | sed 's/"library"/"library-gcc"/' >> $OUT 2>>chain.err
    OMP_NUM_THREADS=$th $PIN $PY -I bench_py.py graphblas $nee $REPS $th >> $OUT 2>>chain.err
  done
  OMP_NUM_THREADS=1 $PIN $PY -I bench_py.py scipy $nee $REPS >> $OUT 2>>chain.err
done
echo DONE >> $OUT
