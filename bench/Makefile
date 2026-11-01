# Benchmark and test builds of the library (same flags as CMakeLists.txt Release, plus the test drivers).
CXX ?= g++
R := ..
CEREAL_INC ?= $(R)/build/_deps/cereal-src/include
CXXFLAGS ?= -O3 -std=c++20 -march=native -fopenmp -DNDEBUG
INC := -I$(R) -I$(R)/pfr_non_boost-master/include -I$(CEREAL_INC) -I/usr/local/include
LIBS := -ltbb
CORE := $(R)/o2m.cpp $(R)/m2m.cpp $(R)/graph.cpp
ALL := $(CORE) $(R)/mm2m.cpp $(R)/thing.cpp

all: bench_lib bench_tables bench_graph running_example unittests RemeshProgram test_graph test_synth test_order

bench_lib: bench_lib.cpp $(CORE) $(R)/o2m.hpp $(R)/m2m.hpp $(R)/seque.hpp
	$(CXX) $(CXXFLAGS) $(INC) bench_lib.cpp $(CORE) $(LIBS) -o $@

bench_tables: bench_tables.cpp $(CORE) $(R)/o2m.hpp $(R)/m2m.hpp $(R)/seque.hpp
	$(CXX) $(CXXFLAGS) $(INC) bench_tables.cpp $(CORE) $(LIBS) -o $@

bench_graph: bench_graph.cpp $(CORE) $(R)/graph.hpp $(R)/o2m.hpp $(R)/m2m.hpp
	$(CXX) $(CXXFLAGS) $(INC) bench_graph.cpp $(CORE) $(LIBS) -o $@

test_synth: test_synth.cpp $(ALL) $(R)/catalogue.hpp $(R)/zoo.hpp $(R)/graph.hpp
	$(CXX) $(CXXFLAGS) $(INC) test_synth.cpp $(ALL) $(LIBS) -o $@

test_order: test_order.cpp $(CORE) $(R)/o2m.hpp $(R)/seque.hpp
	$(CXX) $(CXXFLAGS) $(INC) test_order.cpp $(CORE) $(LIBS) -o $@

test_graph: test_graph.cpp $(CORE) $(R)/graph.hpp $(R)/o2m.hpp $(R)/m2m.hpp
	$(CXX) $(CXXFLAGS) $(INC) test_graph.cpp $(CORE) $(LIBS) -o $@

running_example: running_example.cpp $(ALL) $(R)/o2m.hpp $(R)/m2m.hpp $(R)/mm2m.hpp $(R)/seque.hpp $(R)/zoo.hpp
	$(CXX) $(CXXFLAGS) $(INC) running_example.cpp $(ALL) $(LIBS) -o $@

unittests: $(R)/main.cpp $(ALL) $(R)/testeo2m.hpp $(R)/testm2m.hpp $(R)/testmm2m.hpp $(R)/outputtoensight.hpp
	$(CXX) $(CXXFLAGS) $(INC) $(R)/main.cpp $(ALL) $(LIBS) -o $@

RemeshProgram: $(R)/remesh_main.cpp $(R)/remesh.cpp $(R)/meshquality.cpp $(ALL) $(R)/remesh.hpp $(R)/meshquality.hpp
	$(CXX) $(CXXFLAGS) $(INC) $(R)/remesh_main.cpp $(R)/remesh.cpp $(R)/meshquality.cpp $(ALL) $(LIBS) -o $@

clean:
	rm -f bench_lib bench_tables bench_graph running_example unittests RemeshProgram test_graph test_synth test_order
