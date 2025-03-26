#include <cassert>
#include <iostream>
#include <unordered_set>
#include "entity.hpp"
#include "godoftypes.hpp"
#include "relationmatrix.hpp"
#include "relationmodel.hpp"
#include "relationonetomany.hpp"
#include "sek.hpp"
#include "test.hpp"
#include "typsek.hpp"
using namespace std;

int main(int argc, char *argv[])
{
    entity hascoordinates;
    entity haslength;
    entity hasarea;
    entity hasvolume;
    entity node, edge, tri, quad, tet, hex, wedge;
    settype(hascoordinates, 0);
    settype(haslength, 1);
    settype(hasarea, 2);
    settype(hasvolume, 3);
    settype(node, 4);
    settype(edge, 5);
    settype(tri, 6);
    settype(quad, 7);
    settype(tet, 8);
    settype(hex, 9);
    settype(wedge, 10);

    relationmatrix rm;
    setnumberoftypes(rm, 11);
    setsymmetrygroup(rm, node.type, hascoordinates.type, {{}});
    setsymmetrygroup(rm, edge.type, node.type, {{0, 1}, {1, 0}});
    setsymmetrygroup(rm, edge.type, haslength.type, {{}});
    setsymmetrygroup(rm, tri.type, node.type, {{0, 1, 2}, {1, 2, 0}, {2, 0, 1}});
    setsymmetrygroup(rm, tri.type, hasarea.type, {{}});
    setsymmetrygroup(rm, quad.type, node.type, {{0, 1, 2, 3}, {1, 2, 3, 0}, {2, 3, 0, 1}, {3, 0, 1, 2}});
    setsymmetrygroup(rm, quad.type, hasarea.type, {{}});
    setsymmetrygroup(rm, tet.type, node.type,
                     {{0, 1, 2, 3},
                      {1, 2, 0, 3},
                      {2, 0, 1, 3},
                      {0, 3, 1, 2},
                      {3, 1, 0, 2},
                      {0, 2, 3, 1},
                      {0, 3, 2, 1},
                      {3, 0, 1, 2},
                      {2, 1, 3, 0},
                      {1, 3, 0, 2},
                      {3, 2, 0, 1},
                      {2, 0, 3, 1}});
    setsymmetrygroup(rm, tet.type, hasvolume.type, {{}});
    setsymmetrygroup(
            rm, hex.type, node.type,
            {{0, 1, 2, 3, 4, 5, 6, 7}, {1, 2, 3, 0, 5, 6, 7, 4}, {2, 3, 0, 1, 6, 7, 4, 5}, {3, 0, 1, 2, 7, 4, 5, 6},
             {4, 0, 3, 7, 5, 1, 2, 6}, {5, 4, 7, 6, 1, 0, 3, 2}, {1, 5, 6, 2, 0, 4, 7, 3}, {2, 6, 7, 3, 1, 5, 4, 0},
             {3, 2, 6, 7, 0, 1, 5, 4}, {7, 6, 5, 4, 3, 2, 1, 0}, {4, 5, 1, 0, 7, 6, 2, 3}, {0, 3, 7, 4, 1, 2, 6, 5},
             {0, 4, 5, 1, 3, 7, 6, 2}, {4, 5, 1, 0, 7, 6, 2, 3}, {5, 1, 0, 4, 6, 2, 3, 7}, {1, 0, 4, 5, 2, 3, 7, 6},
             {2, 1, 5, 6, 3, 0, 4, 7}, {6, 5, 4, 7, 2, 1, 0, 3}, {7, 4, 0, 3, 6, 5, 1, 2}, {3, 7, 6, 2, 0, 4, 5, 1},
             {1, 0, 3, 2, 5, 4, 7, 6}, {2, 1, 0, 3, 6, 5, 4, 7}, {3, 2, 1, 0, 7, 6, 5, 4}, {0, 3, 2, 1, 4, 7, 6, 5}});
    setsymmetrygroup(rm, hex.type, hasvolume.type, {{}});
    setsymmetrygroup(rm, wedge.type, node.type,
                     {{0, 1, 2, 3, 4, 5},
                      {1, 2, 0, 4, 5, 3},
                      {2, 0, 1, 5, 3, 4},
                      {0, 2, 1, 3, 5, 4},
                      {2, 1, 0, 5, 4, 3},
                      {1, 0, 2, 4, 3, 5}});
    setsymmetrygroup(rm, wedge.type, hasvolume.type, {{}});
    appendnodesofonetype(node, hascoordinates.type, {8});
    appendnodesofonetype(edge, node.type, {3, 2});
    appendnodesofonetype(edge, haslength.type, {0});
    appendnodesofonetype(tri, node.type, {5, 4, 3});
    appendnodesofonetype(tri, hasarea.type, {0});
    appendnodesofonetype(quad, node.type, {6, 5, 4, 3});
    appendnodesofonetype(quad, hasarea.type, {0});
    appendnodesofonetype(tet, node.type, {5, 1, 0, 2});
    appendnodesofonetype(tet, hasvolume.type, {0});
    appendnodesofonetype(hex, node.type, {8, 7, 6, 5, 4, 3, 2, 1});
    appendnodesofonetype(hex, hasvolume.type, {0});
    appendnodesofonetype(wedge, node.type, {5, 4, 3, 2, 1, 0});
    appendnodesofonetype(wedge, hasvolume.type, {0});
    insertentity(rm, haslength);
    insertentity(rm, hasarea);
    insertentity(rm, hasvolume);
    insertentity(rm, node);
    insertentity(rm, edge);
    insertentity(rm, tri);
    insertentity(rm, quad);
    insertentity(rm, tet);
    insertentity(rm, hex);
    closeelementnoderelation(rm, quad.type, node.type);
    std::cout << "tet" << rm.operator()(tet.type, node.type).nodesfromelement.lnods << std::endl;
    std::cout << "hex" << rm.operator()(hex.type, node.type).nodesfromelement.lnods << std::endl;
}
