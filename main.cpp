#include <cassert>
#include <iostream>
#include <unordered_set>
#include "godoftypes.hpp"
#include "mm2m.hpp"
#include "o2m.hpp"
#include "seque.hpp"
#include "test.hpp"
#include "testm2m.hpp"
#include "testmm2m.hpp"
#include "thing.hpp"
#include "typsek.hpp"
using namespace std;

int main(int argc, char *argv[])
{
    testsek();
    testm2m();
    testmm2m();
}
