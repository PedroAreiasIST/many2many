#include "o2m.hpp"
#include "outputtoensight.hpp"
#include "testeo2m.hpp"
#include "testm2m.hpp"
#include "testmm2m.hpp"
#include <cassert>
#include <omp.h>
using namespace std;

int main(int argc, char *argv[]) {
  //   ensightoutput(0, "teste",100,vector<array<double,3>>(),20,0,0,0,)
  // testseque();
  omp_set_num_threads(16);
  // testm2m();
  testmm2m();
  //  testeo2m();
}
