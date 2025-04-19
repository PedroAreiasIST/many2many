#include "superstruct.hpp"
#include "testm2m.hpp"
#include "testmm2m.hpp"
#include "mm2m.hpp"
#include "typseque.hpp"
#include <cassert>

using namespace std;

struct node
{
    double x, y, z;
};

PFR_FUNCTIONS_FOR(node)

struct isanelement
{
};

PFR_FUNCTIONS_FOR(isanelement)

struct edge
{
};

PFR_FUNCTIONS_FOR(edge)

struct tet
{
};

PFR_FUNCTIONS_FOR(tet)

struct wedge
{
};

PFR_FUNCTIONS_FOR(wedge)

int main(int argc, char *argv[])
{
    using ElementTypes = typseque<edge, tet, wedge>;
    using OtherTypes = typseque<isanelement, node>;
    using MeshType = typsequemergetype<ElementTypes, OtherTypes>;
    using TypeManager = typsequetostructtype<MeshType, superstruct>;
    TypeManager mesh;
    mm2m matrix;
    setnumberoftypes(matrix, mesh.Size);
    auto e0 = append(getsequence<isanelement>(mesh), isanelement{});
    auto n0 = append(getsequence<node>(mesh), node({0, 0, 0}));
    auto n1 = append(getsequence<node>(mesh), node({1, 0, 0}));
    auto n2 = append(getsequence<node>(mesh), node({2, 0, 0}));
    auto n3 = append(getsequence<node>(mesh), node({2, 1, 0}));
    auto n4 = append(getsequence<node>(mesh), node({1, 1, 0}));
    auto n5 = append(getsequence<node>(mesh), node({3, 1, 0}));
    auto n6 = append(getsequence<node>(mesh), node({2, 1, 1}));
    auto tet0 = append(getsequence<tet>(mesh), tet());
    auto edge0 = append(getsequence<edge>(mesh), edge());
    auto edge1 = append(getsequence<edge>(mesh), edge());
    auto wedge0 = append(getsequence<wedge>(mesh), wedge());

    appendelement(matrix(getnumber<tet, TypeManager>(), getnumber<node, TypeManager>()), {4, 2, 6, 0});
    appendelement(matrix(getnumber<tet, TypeManager>(), getnumber<isanelement, TypeManager>()), {0});
    appendelement(matrix(getnumber<edge, TypeManager>(), getnumber<node, TypeManager>()), {3, 0});
    appendelement(matrix(getnumber<edge, TypeManager>(), getnumber<isanelement, TypeManager>()), {0});
    appendelement(matrix(getnumber<edge, TypeManager>(), getnumber<node, TypeManager>()), {3, 1});
    appendelement(matrix(getnumber<edge, TypeManager>(), getnumber<isanelement, TypeManager>()), {0});
    appendelement(matrix(getnumber<wedge, TypeManager>(), getnumber<node, TypeManager>()), {6, 3, 5, 1, 2, 4});
    appendelement(matrix(getnumber<wedge, TypeManager>(), getnumber<isanelement, TypeManager>()), {0});
    marktoerase(matrix, getnumber<node, TypeManager>(), 3);
    std::array<double, 3> d3{1.0, 2.0, 3.0};
    std::cout << d3 << std::endl;
    node mmm{0, 2, 4};
    std::cout << mmm << std::endl;
    std::cout << "   mesh=" << mesh << std::endl;
    TypeManager another;
    another = mesh;
    std::cout << "another=" << another << std::endl;
    //  std::cout << "matrix=" << matrix << std::endl;
    //  testm2m();
    //  testmm2m();
}
