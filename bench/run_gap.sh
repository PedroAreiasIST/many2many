#!/bin/bash
# GAP benchmark suite (Beamer, Asanovic, Patterson; github.com/sbeamer/gapbs) on the node graph of the
# H8 meshes of bench_graph.cpp: direction-optimizing breadth-first search from node 0 and Afforest
# connected components, with the same thread counts and pinning as the library runs. The graph is
# written by dump_sg (serialized .sg) so that both codes traverse exactly the same CSR.
# Env: GAPBS (directory with bfs and cc), DUMP (dump_sg binary), NEES, THREADS, PIN, REPS, OUT, SGDIR.
cd "$(dirname "$0")"
GAPBS=${GAPBS:-$HOME/gapbs}
DUMP=${DUMP:-./dump_sg}
NEES=${NEES:-"100 200 300"}
THREADS=${THREADS:-"16 1"}
PIN=${PIN:-"taskset -c 0-15"}
REPS=${REPS:-5}
OUT=${OUT:-results_gap.jsonl}
SGDIR=${SGDIR:-.}
export OMP_PROC_BIND=close OMP_PLACES=cores
: > $OUT
med() { sort -g | awk '{a[NR]=$1} END{print a[int((NR+1)/2)]}'; }
for nee in $NEES; do
  sg=$SGDIR/h8_$nee.sg
  OMP_NUM_THREADS=$(nproc) $PIN $DUMP $nee $sg >> gap.err 2>&1 || { echo "dump failed for nee $nee" >> gap.err; continue; }
  nel=$((nee*nee*nee)); nn=$(((nee+1)*(nee+1)*(nee+1)))
  for th in $THREADS; do
    tb=$(OMP_NUM_THREADS=$th $PIN $GAPBS/bfs -f $sg -r 0 -n $REPS 2>>gap.err | tee -a gap.log | awk '/^Trial Time/{print $NF}' | med)
    tc=$(OMP_NUM_THREADS=$th $PIN $GAPBS/cc -f $sg -n $REPS 2>>gap.err | tee -a gap.log | awk '/^Trial Time/{print $NF}' | med)
    echo "{\"backend\": \"gapbs\", \"threads\": $th, \"nee\": $nee, \"nel\": $nel, \"nn\": $nn, \"t_bfs\": ${tb:-null}, \"t_cc\": ${tc:-null}}" >> $OUT
  done
  rm -f $sg
done
echo DONE >> $OUT
