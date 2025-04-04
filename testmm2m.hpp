//
// Created by pedro on 4/3/25.
//

#ifndef TESTMMM_HPP
#define TESTMMM_HPP
#include "basics.hpp"
#include "mm2m.hpp"
#include "symmetries.hpp"
#include "thing.hpp"
using namespace std;
void testmm2m()
{
    // symmetry groups
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
    // thing skeletons
    thing isanelements[1];
    thing haslengths[1];
    thing hasareas[1];
    thing hasvolumes[1];
    thing nodes[11], points[1], edges[1], tris[2], quads[1], tets[1], hexs[1], wedges[1];
    thing elementgroups[2];
    // assigntype numbers
    enum typenumbers : size_t
    {
        isanelement=0,
        haslength,
        hasarea,
        hasvolume,
        node,
        point,
        edge,
        tri,
        quad,
        tet,
        hex,
        wedge,
        elementgroup
    };
    settypenumber(isanelements[0], 0);
    settypenumber(haslengths[0], 1);
    settypenumber(hasareas[0], 2);
    settypenumber(hasvolumes[0], 3);
    for (thing & anode : nodes)
    {
        settypenumber(anode,4);
    }
    settypenumber(points[0], 5);
    settypenumber(edges[0], 6);
    settypenumber(tris[0], 7);
    settypenumber(tris[1],7);
    settypenumber(quads[0],8);
    settypenumber(tets[0],9);
    settypenumber(hexs[0],10);
    settypenumber(wedges[0],11);
    settypenumber(elementgroups[0],12);
    settypenumber(elementgroups[1],12);
    // symmetry groups
    mm2m rm;
    setnumberoftypes(rm, 13);
    setsymmetrygroup(rm,node,node,{{0}});
    setsymmetrygroup(rm, point, node, {{0}});
    setsymmetrygroup(rm, point, isanelement, {{0}});
    setsymmetrygroup(rm, edge, node, {{0, 1}, {1, 0}});
    setsymmetrygroup(rm, edge, haslength, {{0}});
    setsymmetrygroup(rm, edge, isanelement, {{0}});
    setsymmetrygroup(rm, tri, node, t3sym);
    setsymmetrygroup(rm, tri, hasarea, {{0}});
    setsymmetrygroup(rm, tri, isanelement, {{0}});
    setsymmetrygroup(rm, quad, node, q4sym);
    setsymmetrygroup(rm, quad, hasarea, {{0}});
    setsymmetrygroup(rm, quad, isanelement, {{0}});
    setsymmetrygroup(rm, tet, node, tet4sym);
    setsymmetrygroup(rm, tet, hasvolume, {{0}});
    setsymmetrygroup(rm, tet, isanelement, {{0}});
    setsymmetrygroup(rm, hex, node, h8symm);
    setsymmetrygroup(rm, hex, hasvolume, {{0}});
    setsymmetrygroup(rm, hex, isanelement, {{0}});
    setsymmetrygroup(rm, wedge, node, wedge6);
    setsymmetrygroup(rm, wedge, hasvolume, {{0}});
    setsymmetrygroup(rm, wedge, isanelement, {{0}});
    setsymmetrygroup(rm, elementgroup, isanelement, {{}});

    // specialization of things
    appendnodesofonetype(nodes[0],node,{0});
    appendnodesofonetype(point, isanelement, {0});
    appendnodesofonetype(edge, node.type, {0, 1});
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
    appendnodesofonetype(elementgroup, isanelement.type, {0});
    insertathing(rm, isanelement);
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
    closeeverything(rm);
    std::cout << "type order" << typetoporder(rm) << std::endl;
    std::cout << " rm" << rm(tet.type, node.type).elementsfromnode << std::endl;
    std::cout << "rm nodelocation" << rm(tet.type, node.type).nodelocation << std::endl;
    std::cout << "wedge" << rm(wedge.type, node.type).nodesfromelement.lnods << std::endl;
    std::cout << "hex2 " << rm.operator()(hex.type, node.type).nodesfromelement.lnods << std::endl;
}
#endif // TESTMMM_HPP
