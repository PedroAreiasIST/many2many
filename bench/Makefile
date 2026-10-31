# Benchmark and test builds of the library (same flags as CMakeLists.txt Release, plus the test drivers).
CXX ?= g++
R := ..
CEREAL_INC ?= /tmp/claude-1000/-home-pedro-Desktop-NovosPapers-remesh-tosendijnme/1f833e97-2ab4-4f0c-81f4-5620added823/scratchpad/ext/many2many/bld/_deps/cereal-src/include
CXXFLAGS ?= -O3 -std=c++20 -march=native -fopenmp -DNDEBUG
INC := -I$(R) -I$(R)/pfr_non_boost-master/include -I$(CEREAL_INC) -I/usr/local/include
LIBS := -ltbb
CORE := $(R)/o2m.cpp $(R)/m2m.cpp
ALL := $(CORE) $(R)/mm2m.cpp $(R)/thing.cpp

all: bench_lib bench_tables running_example unittests RemeshProgram

bench_lib: bench_lib.cpp $(CORE) $(R)/o2m.hpp $(R)/m2m.hpp $(R)/seque.hpp
	$(CXX) $(CXXFLAGS) $(INC) bench_lib.cpp $(CORE) $(LIBS) -o $@

bench_tables: bench_tables.cpp $(CORE) $(R)/o2m.hpp $(R)/m2m.hpp $(R)/seque.hpp
	$(CXX) $(CXXFLAGS) $(INC) bench_tables.cpp $(CORE) $(LIBS) -o $@

running_example: running_example.cpp $(ALL) $(R)/o2m.hpp $(R)/m2m.hpp $(R)/mm2m.hpp $(R)/seque.hpp $(R)/zoo.hpp
	$(CXX) $(CXXFLAGS) $(INC) running_example.cpp $(ALL) $(LIBS) -o $@

unittests: $(R)/main.cpp $(ALL) $(R)/testeo2m.hpp $(R)/testm2m.hpp $(R)/testmm2m.hpp $(R)/outputtoensight.hpp
	$(CXX) $(CXXFLAGS) $(INC) $(R)/main.cpp $(ALL) $(LIBS) -o $@

RemeshProgram: $(R)/remesh_main.cpp $(R)/remesh.cpp $(R)/meshquality.cpp $(ALL) $(R)/remesh.hpp $(R)/meshquality.hpp
	$(CXX) $(CXXFLAGS) $(INC) $(R)/remesh_main.cpp $(R)/remesh.cpp $(R)/meshquality.cpp $(ALL) $(LIBS) -o $@

clean:
	rm -f bench_lib bench_tables running_example unittests RemeshProgram
