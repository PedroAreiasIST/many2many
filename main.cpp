#include "godoftypes.hpp"
#include "mm2m.hpp"
#include "o2m.hpp"
#include "seque.hpp"
#include "test.hpp"
#include "testeo2m.hpp"
#include "testm2m.hpp"
#include "testmm2m.hpp"
#include "thing.hpp"
#include "typsek.hpp"
#include <cassert>
#include <iostream>
#include <unordered_set>
using namespace std;

int main(int argc, char *argv[]) {
  // testsek();
  testm2m();
  testmm2m();
  testeo2m();
}
