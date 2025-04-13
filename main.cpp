#include "o2m.hpp"
#include "testeo2m.hpp"
#include "testm2m.hpp"
#include "testmm2m.hpp"
#include <cassert>
#include <omp.h>
using namespace std;

int main(int argc, char *argv[])
{
    // testseque();
    omp_set_num_threads(32);
    o2m oom;
    testm2m();
    testmm2m();
    testeo2m();
}
