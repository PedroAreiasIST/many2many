#include "superstruct.hpp"
#include "testm2m.hpp"
#include "testmm2m.hpp"
#include "mm2m.hpp"
#include "typseque.hpp"
#include <cassert>
#include "zoo.hpp"

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
    zoo<MeshType> garden;
    using TypeManager = typsequetostructtype<MeshType, superstruct>;
    TypeManager mesh;
    mm2m matrix;
    setnumberoftypes(matrix, TypeManager::Size);

    int e0 = appendnode<isanelement>(mesh);
    int n0 = appendnode<node>(mesh, 0., 0., 0.);
    int n1 = appendnode<node>(mesh, 1., 0., 0.);
    int n2 = appendnode<node>(mesh, 2., 0., 0.);
    int n3 = appendnode<node>(mesh, 2., 1., 0.);
    int n4 = appendnode<node>(mesh, 1., 1., 0.);
    int n5 = appendnode<node>(mesh, 3., 1., 0.);
    int n6 = appendnode<node>(mesh, 2., 1., 1.);
    int tet0 = appendnode<tet>(mesh);
    int edge0 = appendnode<edge>(mesh);
    int edge1 = appendnode<edge>(mesh);
    int wedge0 = appendnode<wedge>(mesh);

    appendelement<TypeManager, tet, node>(matrix, {4, 2, 6, 0});
    appendelement<TypeManager, tet, isanelement>(matrix, {0});
    appendelement<TypeManager, edge, node>(matrix, {3, 0});
    appendelement<TypeManager, edge, isanelement>(matrix, {0});
    appendelement<TypeManager, edge, node>(matrix, {3, 1});
    appendelement<TypeManager, edge, isanelement>(matrix, {0});
    appendelement<TypeManager, wedge, node>(matrix, {6, 3, 5, 1, 2, 4});
    appendelement<TypeManager, wedge, isanelement>(matrix, {0});

    marktoerase(matrix, getnumber<node, TypeManager>(), 3);

    std::array<double, 3> d3{1.0, 2.0, 3.0};
    std::cout << d3 << std::endl;
    node mmm{0, 2, 4};
    std::cout << mmm << std::endl;
    std::cout << "   mesh=" << mesh << std::endl;
    TypeManager another;
    another = mesh;
    std::cout << "another=" << another << std::endl;
    testmm2m();
}
