#include <cassert>
#include <iostream>
#include <unordered_set>
#include "godoftypes.hpp"
#include "one2many.hpp"
#include "relationmatrix.hpp"
#include "seque.hpp"
#include "test.hpp"
#include "testmm.hpp"
#include "thing.hpp"
#include "typsek.hpp"
using namespace std;

int main(int argc, char *argv[])
{
    const seque<seque<size_t>> h8symm{
            {0, 1, 2, 3, 4, 5, 6, 7}, {1, 2, 3, 0, 5, 6, 7, 4}, {2, 3, 0, 1, 6, 7, 4, 5}, {3, 0, 1, 2, 7, 4, 5, 6},
            {4, 0, 3, 7, 5, 1, 2, 6}, {5, 4, 7, 6, 1, 0, 3, 2}, {1, 5, 6, 2, 0, 4, 7, 3}, {2, 6, 7, 3, 1, 5, 4, 0},
            {3, 2, 6, 7, 0, 1, 5, 4}, {7, 6, 5, 4, 3, 2, 1, 0}, {4, 5, 1, 0, 7, 6, 2, 3}, {0, 3, 7, 4, 1, 2, 6, 5},
            {0, 4, 5, 1, 3, 7, 6, 2}, {4, 5, 1, 0, 7, 6, 2, 3}, {5, 1, 0, 4, 6, 2, 3, 7}, {1, 0, 4, 5, 2, 3, 7, 6},
            {2, 1, 5, 6, 3, 0, 4, 7}, {6, 5, 4, 7, 2, 1, 0, 3}, {7, 4, 0, 3, 6, 5, 1, 2}, {3, 7, 6, 2, 0, 4, 5, 1},
            {1, 0, 3, 2, 5, 4, 7, 6}, {2, 1, 0, 3, 6, 5, 4, 7}, {3, 2, 1, 0, 7, 6, 5, 4}, {0, 3, 2, 1, 4, 7, 6, 5}};
    const seque<seque<size_t>> t3sym{{0, 1, 2}, {1, 2, 0}, {2, 0, 1}};
    const seque<seque<size_t>> q4sym{{0, 1, 2, 3}, {1, 2, 3, 0}, {2, 3, 0, 1}, {3, 0, 1, 2}};
    const seque<seque<size_t>> tet4sym{{0, 1, 2, 3}, {1, 2, 0, 3}, {2, 0, 1, 3}, {0, 3, 1, 2},
                                       {3, 1, 0, 2}, {0, 2, 3, 1}, {0, 3, 2, 1}, {3, 0, 1, 2},
                                       {2, 1, 3, 0}, {1, 3, 0, 2}, {3, 2, 0, 1}, {2, 0, 3, 1}};
    const seque<seque<size_t>> wedge6{{0, 1, 2, 3, 4, 5}, {1, 2, 0, 4, 5, 3}, {2, 0, 1, 5, 3, 4},
                                      {0, 2, 1, 3, 5, 4}, {2, 1, 0, 5, 4, 3}, {1, 0, 2, 4, 3, 5}};

    testmany2many();
    // skeleton
    thing isanelement;
    thing hascoordinates;
    thing haslength;
    thing hasarea;
    thing hasvolume;
    thing node, point, edge, tri, quad, tet, hex, wedge;
    thing elementgroup;
    settype(isanelement, 0);
    settype(hascoordinates, 1);
    settype(haslength, 2);
    settype(hasarea, 3);
    settype(hasvolume, 4);
    settype(node, 5);
    settype(point, 6);
    settype(edge, 7);
    settype(tri, 8);
    settype(quad, 9);
    settype(tet, 10);
    settype(hex, 11);
    settype(wedge, 12);
    settype(elementgroup, 13);
    relationmatrix rm;
    setnumberoftypes(rm, 14);
    setsymmetrygroup(rm, node.type, hascoordinates.type, {{0}});
    setsymmetrygroup(rm, point.type, node.type, {{0}});
    setsymmetrygroup(rm, point.type, isanelement.type, {{0}});
    setsymmetrygroup(rm, edge.type, node.type, {{0, 1}, {1, 0}});
    setsymmetrygroup(rm, edge.type, haslength.type, {{0}});
    setsymmetrygroup(rm, edge.type, isanelement.type, {{0}});
    setsymmetrygroup(rm, tri.type, node.type, t3sym);
    setsymmetrygroup(rm, tri.type, hasarea.type, {{0}});
    setsymmetrygroup(rm, tri.type, isanelement.type, {{0}});
    setsymmetrygroup(rm, quad.type, node.type, q4sym);
    setsymmetrygroup(rm, quad.type, hasarea.type, {{0}});
    setsymmetrygroup(rm, quad.type, isanelement.type, {{0}});
    setsymmetrygroup(rm, tet.type, node.type, tet4sym);
    setsymmetrygroup(rm, tet.type, hasvolume.type, {{0}});
    setsymmetrygroup(rm, tet.type, isanelement.type, {{0}});
    setsymmetrygroup(rm, hex.type, node.type, h8symm);
    setsymmetrygroup(rm, hex.type, hasvolume.type, {{0}});
    setsymmetrygroup(rm, hex.type, isanelement.type, {{0}});
    setsymmetrygroup(rm, wedge.type, node.type, wedge6);
    setsymmetrygroup(rm, wedge.type, hasvolume.type, {{0}});
    setsymmetrygroup(rm, wedge.type, isanelement.type, {{0}});
    setsymmetrygroup(rm, elementgroup.type, node.type, {{}});

    appendnodesofonetype(node, hascoordinates.type, {8});
    appendnodesofonetype(point, node.type, {0});
    appendnodesofonetype(point, isanelement.type, {0});
    appendnodesofonetype(edge, node.type, {3, 2});
    appendnodesofonetype(edge, haslength.type, {0});
    appendnodesofonetype(edge, isanelement.type, {0});
    appendnodesofonetype(tri, node.type, {5, 4, 3});
    appendnodesofonetype(tri, hasarea.type, {0});
    appendnodesofonetype(tri, isanelement.type, {0});
    appendnodesofonetype(quad, node.type, {6, 5, 4, 3});
    appendnodesofonetype(quad, hasarea.type, {0});
    appendnodesofonetype(quad, isanelement.type, {0});
    appendnodesofonetype(tet, node.type, {5, 1, 0, 2});
    appendnodesofonetype(tet, hasvolume.type, {0});
    appendnodesofonetype(tet, isanelement.type, {0});
    appendnodesofonetype(hex, node.type, {8, 7, 6, 5, 4, 3, 2, 1});
    appendnodesofonetype(hex, hasvolume.type, {0});
    appendnodesofonetype(hex, isanelement.type, {0});
    appendnodesofonetype(wedge, node.type, {5, 4, 3, 2, 1, 0});
    appendnodesofonetype(wedge, hasvolume.type, {0});
    appendnodesofonetype(wedge, isanelement.type, {0});

    insertathing(rm, isanelement);
    insertathing(rm, hascoordinates);
    insertathing(rm, haslength);
    insertathing(rm, hasarea);
    insertathing(rm, hasvolume);
    insertathing(rm, node);
    insertathing(rm, point);
    insertathing(rm, edge);
    insertathing(rm, tri);
    insertathing(rm, quad);
    insertathing(rm, tet);
    insertathing(rm, hex);
    insertathing(rm, wedge);
    insertathing(rm, elementgroup);
    std::cout << "tet " << rm.operator()(tet.type, node.type).nodesfromelement.lnods << std::endl;
    std::cout << "hex " << rm.operator()(hex.type, node.type).nodesfromelement.lnods << std::endl;
    thing hex2;
    settype(hex2, hex.type);
    appendnodesofonetype(hex2, node.type, {8, 7, 6, 5, 4, 3, 2, 9});
    insertathing(rm, hex2);
    closeeverything(rm);
    std::cout << " rm" << rm(tet.type, node.type).elementsfromnode << std::endl;
    std::cout << "rm nodelocation" << rm(tet.type, node.type).nodelocation << std::endl;
    std::cout << "wedge" << rm(wedge.type, node.type).nodesfromelement.lnods << std::endl;
    std::cout << "hex2 " << rm.operator()(hex.type, node.type).nodesfromelement.lnods << std::endl;
}
