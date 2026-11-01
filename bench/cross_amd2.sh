#!/bin/bash
# amd2 (AMD Ryzen 9 9950X, 16 cores / 32 threads, 96 GB): builds the drivers with g++ 15 and oneAPI 2025.2
# (icpx, ifx, TBB) and runs run_cross.sh with 16 threads on the 16 physical cores (CPUs 0-15), 32 threads
# on all CPUs, and one thread. Layout: $HOME/m2mbench/{many2many, cereal_inc, gapbs, venv}.
set -e
B=$HOME/m2mbench
cd $B/many2many/bench
command -v icpx >/dev/null 2>&1 || source /opt/intel/oneapi/setvars.sh >/dev/null 2>&1 || true
echo "g++: $(g++ --version | head -1)  icpx: $(command -v icpx)  ifx: $(command -v ifx)"
make CEREAL_INC=$B/cereal_inc bench_lib bench_graph dump_sg bench_f90
(cd $B/gapbs && make bfs cc)
ARCH=-march=native CEREAL_INC=$B/cereal_inc ./build_icpx.sh bench_lib bench_graph dump_sg
ifx -O3 -qopenmp -march=native basfun_bench.f90 bench_f90.f90 -o bench_f90_ifx
LABEL=amd2 THREADS_CHAIN="16 32 1" THREADS_GRAPH="16 32 1" PIN="taskset -c 0-31" \
  NEES="100 200 300" NEES_GRAPH="100 200 300" REPS=5 PY=$B/venv/bin/python GAPBS=$B/gapbs SGDIR=$B ./run_cross.sh
