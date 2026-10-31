#!/bin/bash
# Derived queries: library (icpx, 16 threads pinned) against SciPy csgraph (one thread) on H8 meshes.
cd "$(dirname "$0")"
source /opt/intel/oneapi/setvars.sh >/dev/null 2>&1
REPS=${REPS:-5}
OUT=${OUT:-results_graph.jsonl}; : > $OUT
export OMP_PROC_BIND=close OMP_PLACES=cores
PY=/tmp/claude-1000/-home-pedro-Desktop-NovosPapers-remesh-tosendijnme/1f833e97-2ab4-4f0c-81f4-5620added823/scratchpad/venv/bin/python
for nee in 100 200; do
  OMP_NUM_THREADS=16 taskset -c 0-15 ./bench_graph_icpx $nee $REPS >> $OUT 2>>graph.err
  OMP_NUM_THREADS=1 taskset -c 0-15 ./bench_graph_icpx $nee 3 | sed 's/"library"/"library-1thread"/' >> $OUT 2>>graph.err
  OMP_NUM_THREADS=1 taskset -c 0-15 $PY -I bench_graph.py $nee 3 >> $OUT 2>>graph.err
done
OMP_NUM_THREADS=16 taskset -c 0-15 ./bench_graph_icpx 300 3 >> $OUT 2>>graph.err
OMP_NUM_THREADS=1 taskset -c 0-15 $PY -I bench_graph.py 300 1 >> $OUT 2>>graph.err
# bulk synthesis throughput (faces and edges of hexahedra, boundary) at 1M and 8M elements
for nee in 100 200; do
  OMP_NUM_THREADS=16 taskset -c 0-15 ./test_synth_icpx $nee | grep -E 'H8|TIMER' | sed "s/^/synth nee $nee: /" >> $OUT 2>>graph.err
done
echo DONE >> $OUT
