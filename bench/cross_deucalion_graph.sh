#!/bin/bash
# Deucalion: rebuilds the derived-query driver after a kernel change (components by subgraph sampling), runs its
# checks on the login node and submits the derived-query part of run_cross.sh alone (CHAIN=0 GAP=0).
# Run from a login shell: bash -l cross_deucalion_graph.sh
set -e
B=$HOME/m2mbench
cd $B/many2many/bench
module purge
module load GCC/15.2.0 tbb/2022.3.0-GCCcore-15.2.0
rm -f bench_graph test_graph bench_graph_icpx test_graph_icpx
make CEREAL_INC=$B/cereal_inc CXXFLAGS="-O3 -std=c++20 -march=native -fopenmp -DNDEBUG -static-libstdc++ -static-libgcc" bench_graph test_graph
module load intel-compilers/2025.2.0
ARCH=-march=native CEREAL_INC=$B/cereal_inc ./build_icpx.sh bench_graph test_graph
OMP_NUM_THREADS=8 ./test_graph | tail -1
OMP_NUM_THREADS=8 ./test_graph_icpx | tail -1
cat > $B/m2mgraph.sbatch <<EOF
#!/bin/bash
#SBATCH -A f2026002cncaist_ulx
#SBATCH -p dev-x86
#SBATCH -N 1
#SBATCH --exclusive
#SBATCH --cpus-per-task=128
#SBATCH -t 02:00:00
#SBATCH -J m2mgraph
#SBATCH -o $B/m2mgraph_%j.out
[ -f /etc/profile.d/lmod.sh ] && source /etc/profile.d/lmod.sh
module purge
module load GCC/15.2.0 tbb/2022.3.0-GCCcore-15.2.0 intel-compilers/2025.2.0
cd $B/many2many/bench
rm -f cross_deucalion.done
LABEL=deucalion CHAIN=0 GAP=0 GRAPH=1 THREADS_GRAPH="128 64 16 1" PIN="taskset -c 0-127" \\
  NEES_GRAPH="100 200 300 400" REPS=5 PY=$B/venv/bin/python ./run_cross.sh
EOF
sbatch ${AFTER:+--dependency=afterany:$AFTER} $B/m2mgraph.sbatch   # AFTER=<jobid> queues it after the chain job
