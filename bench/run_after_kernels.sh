#!/bin/bash
# Re-measurement after the kernel changes of 2026-11-01: splitter, order table (getorder and
# indicesfromorder changed), derived queries and synthesis.
cd "$(dirname "$0")"
rm -f kernels.done
./run_remesh.sh
md5sum /tmp/claude-1000/-home-pedro-Desktop-NovosPapers-remesh-tosendijnme/1f833e97-2ab4-4f0c-81f4-5620added823/scratchpad/remesh/cpp2/remeshed.malha > remesh_md5.txt
source /opt/intel/oneapi/setvars.sh >/dev/null 2>&1
export OMP_PROC_BIND=close OMP_PLACES=cores
: > results_order.jsonl
for nee in 100 200 300 400 500; do
  OMP_NUM_THREADS=16 taskset -c 0-15 ./bench_tables_icpx order H8 $nee 5 >> results_order.jsonl 2>> tables.err
done
./run_graph.sh
touch kernels.done
