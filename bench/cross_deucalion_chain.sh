#!/bin/bash
# Deucalion: rebuilds the chain driver after a kernel change (product staging) and submits the chain part of
# run_cross.sh alone (GRAPH=0 GAP=0). AFTER=<jobid> queues it after a running job. Run from a login shell.
set -e
B=$HOME/m2mbench
cd $B/many2many/bench
module purge
module load GCC/15.2.0 tbb/2022.3.0-GCCcore-15.2.0
rm -f bench_lib bench_lib_icpx
make CEREAL_INC=$B/cereal_inc CXXFLAGS="-O3 -std=c++20 -march=native -fopenmp -DNDEBUG -static-libstdc++ -static-libgcc" bench_lib
module load intel-compilers/2025.2.0
ARCH=-march=native CEREAL_INC=$B/cereal_inc ./build_icpx.sh bench_lib
OMP_NUM_THREADS=8 ./bench_lib 30 1 | grep -o '"nnz_product": [0-9]*'
cat > $B/m2mchain.sbatch <<EOF
#!/bin/bash
#SBATCH -A f2026002cncaist_ulx
#SBATCH -p dev-x86
#SBATCH -N 1
#SBATCH --exclusive
#SBATCH --cpus-per-task=128
#SBATCH -t 02:00:00
#SBATCH -J m2mchain
#SBATCH -o $B/m2mchain_%j.out
[ -f /etc/profile.d/lmod.sh ] && source /etc/profile.d/lmod.sh
module purge
module load GCC/15.2.0 tbb/2022.3.0-GCCcore-15.2.0 intel-compilers/2025.2.0
cd $B/many2many/bench
rm -f cross_deucalion.done
LABEL=deucalion CHAIN=1 GAP=0 GRAPH=0 THREADS_CHAIN="128 64 16 1" PIN="taskset -c 0-127" \\
  NEES="100 200 300 400" REPS=5 PY=$B/venv/bin/python ./run_cross.sh
EOF
sbatch ${AFTER:+--dependency=afterany:$AFTER} $B/m2mchain.sbatch
