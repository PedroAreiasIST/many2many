#!/bin/bash
# Deucalion (MACC, x86 partition: 2 x AMD EPYC 7742, 128 cores, 256 GB): builds the drivers on the login node
# (same CPU as the compute nodes) and submits run_cross.sh as one exclusive job on dev-x86.
# Layout: $HOME/m2mbench/{many2many, cereal_inc, gapbs, venv, sg}. Run from a login shell: bash -l cross_deucalion.sh
set -e
B=$HOME/m2mbench
cd $B/many2many/bench
module purge
module load GCC/15.2.0 tbb/2022.3.0-GCCcore-15.2.0
make CEREAL_INC=$B/cereal_inc CXXFLAGS="-O3 -std=c++20 -march=native -fopenmp -DNDEBUG -static-libstdc++ -static-libgcc" \
     F90FLAGS="-O3 -march=native -fopenmp -static-libgfortran -static-libgcc" bench_lib bench_graph dump_sg bench_f90
(cd $B/gapbs && make CXX="g++ -static-libstdc++ -static-libgcc" bfs cc)
module load intel-compilers/2025.2.0
ARCH=-march=native CEREAL_INC=$B/cereal_inc ./build_icpx.sh bench_lib bench_graph dump_sg
ifx -O3 -qopenmp -march=native basfun_bench.f90 bench_f90.f90 -o bench_f90_ifx
mkdir -p $B/sg
cat > $B/m2mcross.sbatch <<EOF
#!/bin/bash
#SBATCH -A f2026002cncaist_ulx
#SBATCH -p dev-x86
#SBATCH -N 1
#SBATCH --exclusive
#SBATCH --cpus-per-task=128
#SBATCH -t 04:00:00
#SBATCH -J m2mcross
#SBATCH -o $B/m2mcross_%j.out
[ -f /etc/profile.d/lmod.sh ] && source /etc/profile.d/lmod.sh
module purge
module load GCC/15.2.0 tbb/2022.3.0-GCCcore-15.2.0 intel-compilers/2025.2.0
cd $B/many2many/bench
LABEL=deucalion THREADS_CHAIN="128 64 16 1" THREADS_GRAPH="128 64 16 1" PIN="taskset -c 0-127" \\
  NEES="100 200 300 400" NEES_GRAPH="100 200 300 400" REPS=5 PY=$B/venv/bin/python GAPBS=$B/gapbs SGDIR=$B/sg ./run_cross.sh
EOF
sbatch $B/m2mcross.sbatch
