#!/bin/bash
# Builds the given Makefile targets with the Intel compiler into <target>_icpx (the g++ binaries keep their names).
cd "$(dirname "$0")"
command -v icpx >/dev/null 2>&1 || source /opt/intel/oneapi/setvars.sh >/dev/null 2>&1
R=..
CEREAL_INC=${CEREAL_INC:-$R/build/_deps/cereal-src/include}
INC="-I$R -I$R/pfr_non_boost-master/include -I$CEREAL_INC -I/usr/local/include"
ARCH=${ARCH:--xHost}   # -march=native on AMD hosts
FLAGS="-O3 -std=c++20 $ARCH -fiopenmp -DNDEBUG"
CORE="$R/o2m.cpp $R/m2m.cpp $R/graph.cpp"
ALL="$CORE $R/mm2m.cpp $R/thing.cpp"
for t in "$@"; do
  case $t in
    bench_lib|bench_tables|bench_graph|test_graph|test_order|dump_sg) src="$t.cpp $CORE";;
    test_synth|running_example) src="$t.cpp $ALL";;
    scatter) src="scatter.cpp";;
    unittests) src="$R/main.cpp $ALL";;
    RemeshProgram) src="$R/remesh_main.cpp $R/remesh.cpp $R/meshquality.cpp $ALL";;
    *) echo "unknown target $t"; exit 1;;
  esac
  echo "icpx $t"; icpx $FLAGS $INC $src -ltbb -o ${t}_icpx || exit 1
done
