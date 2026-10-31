#!/bin/bash
# Splitter timing on the quarter-beam mesh: C++ port (icpx and g++ builds) and the Fortran
# explode driver, 16 threads pinned to the performance cores, 5 runs each.
cd "$(dirname "$0")"
source /opt/intel/oneapi/setvars.sh >/dev/null 2>&1
export OMP_NUM_THREADS=16 OMP_PROC_BIND=close OMP_PLACES=cores
OUT=${OUT:-remesh_timings.txt}; : > $OUT
for b in RemeshProgram_icpx RemeshProgram; do
  echo "== $b" >> $OUT
  for r in 1 2 3 4 5; do
    (cd /tmp/claude-1000/-home-pedro-Desktop-NovosPapers-remesh-tosendijnme/1f833e97-2ab4-4f0c-81f4-5620added823/scratchpad/remesh/cpp2 && rm -f remeshed.malha && taskset -c 0-15 /usr/bin/time -f 'wall %e maxRSS %M' $PWD/../../../../../../../../../home/pedro/Desktop/NovosPapers/remesh/tosendijnme/revision1/many2many/bench/$b 2>&1 | grep -E 'TIMER|wall') | tr '\n' ' ' >> $OUT; echo >> $OUT
  done
done
echo "== fortran explode.exe" >> $OUT
for r in 1 2 3 4 5; do
  (cd /tmp/claude-1000/-home-pedro-Desktop-NovosPapers-remesh-tosendijnme/1f833e97-2ab4-4f0c-81f4-5620added823/scratchpad/remesh/f90x && rm -f remeshed.malha && taskset -c 0-15 /usr/bin/time -f 'wall %e maxRSS %M' /home/pedro/RiderProjects/Numerical/SimPlas/explode.exe 2>&1 | grep -i -E 'parse|split|wall|TIMER|read' | grep -v -i 'quality') | tr '\n' ' ' >> $OUT; echo >> $OUT
done
echo DONE >> $OUT
