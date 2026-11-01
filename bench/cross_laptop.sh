#!/bin/bash
# Laptop of the paper (Intel i9-13900HX, 8 performance cores with SMT = CPUs 0-15): the cross-machine run with
# the pinning of every other measurement of the paper (16 threads on CPUs 0-15, and one thread).
# Env: SCR (scratch with venv/ and gapbs/), defaults to the session scratchpad.
set -e
cd "$(dirname "$0")"
SCR=${SCR:-/tmp/claude-1000/-home-pedro-Desktop-NovosPapers-remesh-tosendijnme/1f833e97-2ab4-4f0c-81f4-5620added823/scratchpad}
command -v icpx >/dev/null 2>&1 || source /opt/intel/oneapi/setvars.sh >/dev/null 2>&1 || true
echo "icpx: $(command -v icpx)  ifx: $(command -v ifx)"
[ -x bench_f90_ifx ] || ifx -O3 -xHost -qopenmp basfun_bench.f90 bench_f90.f90 -o bench_f90_ifx
[ -x bench_f90 ] || make bench_f90
[ -x dump_sg_icpx ] || ./build_icpx.sh dump_sg
LABEL=laptop THREADS_CHAIN="16 1" THREADS_GRAPH="16 1" PIN="taskset -c 0-15" NEES="100 200 300" NEES_GRAPH="100 200 300" \
  REPS=5 PY=$SCR/venv/bin/python GAPBS=$SCR/gapbs SGDIR=$SCR ./run_cross.sh
