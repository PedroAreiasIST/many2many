#!/bin/bash
# One machine of the cross-machine comparison: the same-machine chain (Fortran routines, this library
# with g++ and, when present, with icpx, SuiteSparse:GraphBLAS, SciPy), the derived queries against
# SciPy csgraph and the GAP benchmark suite, with the host's thread counts and pinning.
# Results: results_chain_$LABEL.jsonl, results_graph_$LABEL.jsonl, results_gap_$LABEL.jsonl and
# sysinfo_$LABEL.txt. The binaries are built beforehand (make / build_icpx.sh / the Fortran line below).
# Env: LABEL, THREADS_CHAIN, THREADS_GRAPH, PIN, NEES, NEES_GRAPH, REPS, PY (python with scipy and
# python-graphblas), GAPBS (directory with bfs and cc), SGDIR (scratch for the .sg files), CHAIN=0/1,
# GRAPH=0/1, GAP=0/1, LIB_ICPX (bench_lib_icpx or empty), GRAPH_BIN (bench_graph_icpx or bench_graph),
# DUMP (dump_sg_icpx or dump_sg), F90BIN (bench_f90).
cd "$(dirname "$0")"
LABEL=${LABEL:-$(hostname -s)}
THREADS_CHAIN=${THREADS_CHAIN:-"16 1"}
THREADS_GRAPH=${THREADS_GRAPH:-"16 1"}
PIN=${PIN:-"taskset -c 0-15"}
NEES=${NEES:-"100 200 300"}
NEES_GRAPH=${NEES_GRAPH:-"100 200 300"}
REPS=${REPS:-5}
PY=${PY:-python3}
GAPBS=${GAPBS:-$HOME/gapbs}
SGDIR=${SGDIR:-.}
LIB_ICPX=${LIB_ICPX:-$( [ -x ./bench_lib_icpx ] && echo ./bench_lib_icpx )}
GRAPH_BIN=${GRAPH_BIN:-$( [ -x ./bench_graph_icpx ] && echo ./bench_graph_icpx || echo ./bench_graph )}
DUMP=${DUMP:-$( [ -x ./dump_sg_icpx ] && echo ./dump_sg_icpx || echo ./dump_sg )}
F90BINS=${F90BINS:-"$( [ -x ./bench_f90_ifx ] && echo ./bench_f90_ifx ) $( [ -x ./bench_f90 ] && echo ./bench_f90 )"}   # ifx and gfortran builds, labelled fortran-ifx / fortran-gfortran
export OMP_PROC_BIND=close OMP_PLACES=cores
ERR=cross_$LABEL.err; : > $ERR
{
  echo "label: $LABEL  date: $(date -Is)  host: $(hostname)"
  lscpu | grep -E "Model name|^CPU\(s\)|Thread\(s\) per core|Core\(s\) per socket|Socket|NUMA node\(s\)|L2|L3"
  echo "governor: $(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor 2>/dev/null)"
  free -g | head -2
  echo "pin: $PIN  threads chain: $THREADS_CHAIN  threads graph: $THREADS_GRAPH  reps: $REPS"
  g++ --version | head -1; icpx --version 2>/dev/null | head -1; gfortran --version 2>/dev/null | head -1; ifx --version 2>/dev/null | head -1
  $PY -c "import sys,numpy,scipy,graphblas,suitesparse_graphblas as s;print('python',sys.version.split()[0],'numpy',numpy.__version__,'scipy',scipy.__version__,'python-graphblas',graphblas.__version__,'suitesparse-graphblas',s.__version__)" 2>&1
  (cd $GAPBS && git log -1 --format='gapbs %h %ad' --date=short 2>/dev/null)
} > sysinfo_$LABEL.txt 2>&1

if [ "${CHAIN:-1}" = 1 ]; then
  OUT=results_chain_$LABEL.jsonl; : > $OUT
  for nee in $NEES; do
    for th in $THREADS_CHAIN; do
      for f in $F90BINS; do
        lab=fortran-gfortran; case $f in *_ifx) lab=fortran-ifx;; esac
        OMP_NUM_THREADS=$th $PIN $f $nee $REPS | sed "s/\"fortran\"/\"$lab\"/" >> $OUT 2>>$ERR
      done
      [ -n "$LIB_ICPX" ] && OMP_NUM_THREADS=$th $PIN $LIB_ICPX $nee $REPS | sed 's/"library"/"library-icpx"/' >> $OUT 2>>$ERR
      OMP_NUM_THREADS=$th $PIN ./bench_lib $nee $REPS | sed 's/"library"/"library-gcc"/' >> $OUT 2>>$ERR
      OMP_NUM_THREADS=$th $PIN $PY -I bench_py.py graphblas $nee $REPS $th >> $OUT 2>>$ERR
    done
    OMP_NUM_THREADS=1 $PIN $PY -I bench_py.py scipy $nee $REPS >> $OUT 2>>$ERR
  done
  echo DONE >> $OUT
fi

if [ "${GRAPH:-1}" = 1 ]; then
  OUT=results_graph_$LABEL.jsonl; : > $OUT
  for nee in $NEES_GRAPH; do
    r=$REPS; [ $nee -ge 300 ] && r=3
    for th in $THREADS_GRAPH; do
      OMP_NUM_THREADS=$th $PIN $GRAPH_BIN $nee $r >> $OUT 2>>$ERR
    done
    rs=3; [ $nee -ge 300 ] && rs=1
    OMP_NUM_THREADS=1 $PIN $PY -I bench_graph.py $nee $rs >> $OUT 2>>$ERR
  done
  echo DONE >> $OUT
fi

if [ "${GAP:-1}" = 1 ]; then
  GAPBS=$GAPBS DUMP=$DUMP NEES="$NEES_GRAPH" THREADS="$THREADS_GRAPH" PIN="$PIN" REPS=$REPS OUT=results_gap_$LABEL.jsonl SGDIR=$SGDIR ./run_gap.sh
fi
echo CROSS_DONE > cross_$LABEL.done
