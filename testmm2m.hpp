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
    std::cout << "Test of mm2m" << std::endl;
    // symmetry groups
    const seque<seque<int> > h8symm{
        {0, 1, 2, 3, 4, 5, 6, 7}, {1, 2, 3, 0, 5, 6, 7, 4},
        {2, 3, 0, 1, 6, 7, 4, 5}, {3, 0, 1, 2, 7, 4, 5, 6},
        {4, 0, 3, 7, 5, 1, 2, 6}, {5, 4, 7, 6, 1, 0, 3, 2},
        {1, 5, 6, 2, 0, 4, 7, 3}, {2, 6, 7, 3, 1, 5, 4, 0},
        {3, 2, 6, 7, 0, 1, 5, 4}, {7, 6, 5, 4, 3, 2, 1, 0},
        {4, 5, 1, 0, 7, 6, 2, 3}, {0, 3, 7, 4, 1, 2, 6, 5},
        {0, 4, 5, 1, 3, 7, 6, 2}, {4, 5, 1, 0, 7, 6, 2, 3},
        {5, 1, 0, 4, 6, 2, 3, 7}, {1, 0, 4, 5, 2, 3, 7, 6},
        {2, 1, 5, 6, 3, 0, 4, 7}, {6, 5, 4, 7, 2, 1, 0, 3},
        {7, 4, 0, 3, 6, 5, 1, 2}, {3, 7, 6, 2, 0, 4, 5, 1},
        {1, 0, 3, 2, 5, 4, 7, 6}, {2, 1, 0, 3, 6, 5, 4, 7},
        {3, 2, 1, 0, 7, 6, 5, 4}, {0, 3, 2, 1, 4, 7, 6, 5}
    };
    const seque<seque<int> > t3sym{{0, 1, 2}, {1, 2, 0}, {2, 0, 1}};
    const seque<seque<int> > q4sym{
        {0, 1, 2, 3}, {1, 2, 3, 0}, {2, 3, 0, 1}, {3, 0, 1, 2}
    };
    const seque<seque<int> > tet4sym{
        {0, 1, 2, 3}, {1, 2, 0, 3}, {2, 0, 1, 3},
        {0, 3, 1, 2}, {3, 1, 0, 2}, {0, 2, 3, 1},
        {0, 3, 2, 1}, {3, 0, 1, 2}, {2, 1, 3, 0},
        {1, 3, 0, 2}, {3, 2, 0, 1}, {2, 0, 3, 1}
    };
    const seque<seque<int> > wedge6{
        {0, 1, 2, 3, 4, 5}, {1, 2, 0, 4, 5, 3},
        {2, 0, 1, 5, 3, 4}, {0, 2, 1, 3, 5, 4},
        {2, 1, 0, 5, 4, 3}, {1, 0, 2, 4, 3, 5}
    };
    // thing skeletons
    thing isanelements[2];
    thing nodes[11], points[1], edges[1], tris[2], quads[1], tets[1], hexs[1],
            wedges[1];
    thing elementgroups[2];
    // assigntype numbers
    enum typenumbers : int
    {
        isanelement = 0,
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
    settypenumber(isanelements[1], 1);
    for (thing &anode: nodes)
    {
        settypenumber(anode, 4);
    }
    settypenumber(points[0], 5);
    settypenumber(edges[0], 6);
    settypenumber(tris[0], 7);
    settypenumber(tris[1], 7);
    settypenumber(quads[0], 8);
    settypenumber(tets[0], 9);
    settypenumber(hexs[0], 10);
    settypenumber(wedges[0], 11);
    settypenumber(elementgroups[0], 12);
    settypenumber(elementgroups[1], 12);
    // symmetry groups
    mm2m rm;
    setnumberoftypes(rm, 13);
    setsymmetrygroup(rm, node, node, {{0}});
    setsymmetrygroup(rm, isanelement, isanelement, {{0}});
    setsymmetrygroup(rm, point, node, {{0}});
    setsymmetrygroup(rm, point, isanelement, {{0}});
    setsymmetrygroup(rm, edge, node, {{0, 1}, {1, 0}});
    setsymmetrygroup(rm, edge, isanelement, {{0}});
    setsymmetrygroup(rm, tri, node, t3sym);
    setsymmetrygroup(rm, tri, isanelement, {{0}});
    setsymmetrygroup(rm, quad, node, q4sym);
    setsymmetrygroup(rm, quad, isanelement, {{0}});
    setsymmetrygroup(rm, tet, node, tet4sym);
    setsymmetrygroup(rm, tet, isanelement, {{0}});
    setsymmetrygroup(rm, hex, node, h8symm);
    setsymmetrygroup(rm, hex, isanelement, {{0}});
    setsymmetrygroup(rm, wedge, node, wedge6);
    setsymmetrygroup(rm, wedge, isanelement, {{0}});
    setsymmetrygroup(rm, elementgroup, isanelement, {{}});
    // specialization of things
    appendnodesofonetype(isanelements[0], isanelement, {0});
    appendnodesofonetype(isanelements[1], isanelement, {1});
    for (auto anode: nodes)
    {
        appendnodesofonetype(anode, node, {0});
    }
    appendnodesofonetype(points[0], node, {2});
    appendnodesofonetype(points[0], isanelement, {0});
    appendnodesofonetype(edges[0], node, {0, 1});
    appendnodesofonetype(edges[0], isanelement, {0});
    appendnodesofonetype(tris[0], node, {7, 5, 6});
    appendnodesofonetype(tris[1], node, {0, 9, 10});
    appendnodesofonetype(tris[0], isanelement, {0});
    appendnodesofonetype(tris[1], isanelement, {0});

    appendnodesofonetype(quads[0], node, {9, 7, 6, 10});
    appendnodesofonetype(quads[0], isanelement, {0});

    appendnodesofonetype(tets[0], node, {0, 9, 8, 10});
    appendnodesofonetype(tets[0], isanelement, {1});
    appendnodesofonetype(hexs[0], node, {1, 2, 3, 4, 9, 7, 5, 8});
    appendnodesofonetype(hexs[0], isanelement, {1});
    appendnodesofonetype(wedges[0], node, {5, 4, 3, 2, 1, 0});
    appendnodesofonetype(wedges[0], isanelement, {1});
    appendnodesofonetype(elementgroups[0], isanelement, {0});
    appendnodesofonetype(elementgroups[1], isanelement, {1});

    insertathing(rm, isanelements[0]);
    insertathing(rm, nodes[0]);
    insertathing(rm, points[0]);
    insertathing(rm, edges[0]);
    insertathing(rm, tris[0]);
    insertathing(rm, quads[0]);
    insertathing(rm, tets[0]);
    insertathing(rm, hexs[0]);
    insertathing(rm, wedges[0]);
    insertathing(rm, elementgroups[0]);

    /*  std::cout << "tet " << rm.operator()(tet.type,
      node.type).nfrome.lnods << std::endl; std::cout << "hex " <<
      rm.operator()(hex.type, node.type).nfrome.lnods << std::endl;
      closeeverything(rm);
      std::cout << "type order" << typetoporder(rm) << std::endl;
      std::cout << " rm" << rm(tet.type, node.type).efromn << std::endl;
      std::cout << "rm nodeloc" << rm(tet.type, node.type).nodeloc <<
      std::endl; std::cout << "wedge" << rm(wedge.type,
      node.type).nfrome.lnods << std::endl; std::cout << "hex2 " <<
      rm.operator()(hex.type, node.type).nfrome.lnods << std::endl;
      */
}
#endif // TESTMMM_HPP
