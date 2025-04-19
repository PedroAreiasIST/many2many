#include "superstruct.hpp"
// nclude "testm2m.hpp"
//  nclude "testmm2m.hpp"
#include "mm2m.hpp"
#include "typseque.hpp"
#include <cassert>

using namespace std;

struct node {
  std::array<double, 3> coord;
};
struct isanelement {};
struct tri {
  double thickness;
  size_t formulation;
};
struct quad {
  double thickness;
  size_t formulation;
};

void marktoerase(mm2m &m, int nodetype, int node) {
  append(m.listofmarked, std::make_pair(nodetype, node));
}

int main(int argc, char *argv[]) {
  using ElementTypes = typseque<tri, quad>;
  using OtherTypes = typseque<isanelement, node>;
  using MeshType = typsequemergetype<ElementTypes, OtherTypes>;
  using TypeManager = typsequetostructtype<MeshType, superstruct>;
  TypeManager mesh;
  mm2m matrix;
  setnumberoftypes(matrix, mesh.Size);
  auto e0 = append(getsequence<isanelement>(mesh), isanelement{});
  auto e1 = append(getsequence<isanelement>(mesh), isanelement{});
  auto n0 = append(getsequence<node>(mesh), node({0, 0, 0}));
  auto n1 = append(getsequence<node>(mesh), node({1, 0, 0}));
  auto n2 = append(getsequence<node>(mesh), node({2, 0, 0}));
  auto n3 = append(getsequence<node>(mesh), node({2, 1, 0}));
  auto n4 = append(getsequence<node>(mesh), node({1, 1, 0}));
  auto t0 = append(getsequence<tri>(mesh), tri({0.01, 3}));
  auto q0 = append(getsequence<quad>(mesh), quad(0.02, 4));

  appendelement(
      matrix(getnumber<tri, TypeManager>(), getnumber<node, TypeManager>()),
      {0, 1, 4});
  appendelement(matrix(getnumber<tri, TypeManager>(),
                       getnumber<isanelement, TypeManager>()),
                {0});
  appendelement(
      matrix(getnumber<quad, TypeManager>(), getnumber<node, TypeManager>()),
      {1, 2, 3, 4});
  appendelement(matrix(getnumber<quad, TypeManager>(),
                       getnumber<isanelement, TypeManager>()),
                {1});
  marktoerase(matrix, getnumber<node, TypeManager>(), 3);
  // testm2m();
  //  testmm2m();
}
