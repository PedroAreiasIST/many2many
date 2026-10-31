#!/bin/bash
# Scaling tables of the manuscript with the released library (icpx build), pinned to the eight
# performance cores, 16 threads unless stated; medians of REPS runs. One JSON line per call.
cd "$(dirname "$0")"
source /opt/intel/oneapi/setvars.sh >/dev/null 2>&1
REPS=${REPS:-5}
OUT=${OUT:-results_tables.jsonl}; : > $OUT
PIN="taskset -c 0-15"
export OMP_PROC_BIND=close OMP_PLACES=cores
B=./bench_tables_icpx
# ordering, index maps, compression; operators; cliques; search: H8 1M .. 125M (125M: no product)
for nee in 100 200 300 400 500; do
  OMP_NUM_THREADS=16 $PIN $B order H8 $nee $REPS >> $OUT 2>>tables.err
  OMP_NUM_THREADS=16 $PIN $B cliques H8 $nee $REPS >> $OUT 2>>tables.err
  OMP_NUM_THREADS=16 $PIN $B search H8 $nee 3 >> $OUT 2>>tables.err
done
for nee in 100 200 300 400; do
  OMP_NUM_THREADS=16 $PIN $B ops H8 $nee $REPS >> $OUT 2>>tables.err
  OMP_NUM_THREADS=16 $PIN $B assembly H8 $nee $REPS >> $OUT 2>>tables.err
done
# Q4 1M .. 144M
for nee in 1000 2000 4000 8000 12000; do
  OMP_NUM_THREADS=16 $PIN $B assembly Q4 $nee $REPS >> $OUT 2>>tables.err
  OMP_NUM_THREADS=16 $PIN $B cliques Q4 $nee $REPS >> $OUT 2>>tables.err
done
# thread sweep on the largest meshes: 1..16 threads on the performance cores, 24 and 32 on all cores
for th in 1 2 4 8 16; do
  OMP_NUM_THREADS=$th $PIN $B assembly H8 400 3 >> $OUT 2>>tables.err
  OMP_NUM_THREADS=$th $PIN $B assembly Q4 12000 3 >> $OUT 2>>tables.err
done
for th in 24 32; do
  OMP_NUM_THREADS=$th OMP_PLACES=threads $B assembly H8 400 3 >> $OUT 2>>tables.err
  OMP_NUM_THREADS=$th OMP_PLACES=threads $B assembly Q4 12000 3 >> $OUT 2>>tables.err
done
echo DONE >> $OUT
