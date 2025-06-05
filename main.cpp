#include "mm2m.hpp"
#include "superstruct.hpp"
#include "testeo2m.hpp"
#include "testm2m.hpp"
#include "testmm2m.hpp"
#include "typseque.hpp"
#include "zoo.hpp"
#include <cassert>
#include <fstream>
#include <iostream>
// #include <iostream>

using namespace std;

struct node
{
    void print()
    {
        std::cout << "x,y,z" << std::endl;
    }

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
    node n;
    n.x = 33;
    n.y = 44;
    n.z = 55;
    cout << n << endl;
    using ElementTypes = typseque<edge, tet, wedge>;
    using OtherTypes = typseque<isanelement, node>;
    using MeshType = typsequemergetype<ElementTypes, OtherTypes>;
    zoo<MeshType> garden;
    using TypeManager = typsequetostructtype<MeshType, superstruct>;
    TypeManager mesh;
    mm2m matrix;
    setnumberoftypes(matrix, TypeManager::Size);
    seque<node> &nodes = getsequence<node>(mesh);
    nodes[2].x = 3;
    nodes[2].y = 4;
    nodes[2].z = 5;
    // cout << "Mesh new nodes" << endl;
    cout << getsequence<node>(mesh);
    auto nodetype = getnumber<node, TypeManager>();
    auto edgetype = getnumber<edge, TypeManager>();
    auto tettype = getnumber<tet, TypeManager>();
    auto wedgetype = getnumber<wedge, TypeManager>();
    appendelement<TypeManager, tet, node>(matrix, {4, 2, 6, 0});
    appendelement<TypeManager, tet, isanelement>(matrix, {0});
    appendelement<TypeManager, edge, node>(matrix, {3, 0});
    appendelement<TypeManager, edge, isanelement>(matrix, {0});
    appendelement<TypeManager, edge, node>(matrix, {3, 1});
    appendelement<TypeManager, edge, isanelement>(matrix, {0});
    appendelement<TypeManager, wedge, node>(matrix, {6, 3, 5, 1, 2, 4});
    appendelement<TypeManager, wedge, isanelement>(matrix, {0});
    compress(matrix);
    auto elnode3 = getallelements(matrix, nodetype, 3);
    testmm2m();
    testm2m();
    testeo2m();
}
