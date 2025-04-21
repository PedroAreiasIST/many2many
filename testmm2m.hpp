//
// Created by pedro on 4/3/25.
//

#ifndef TESTMMM_HPP
#define TESTMMM_HPP
#include "basics.hpp"
#include "mm2m.hpp"
#include "symmetries.hpp"
#include "thing.hpp"
#include "outputtoensight.hpp"
#include "zoo.hpp"
#include "typseque.hpp"
using namespace std;

void testmm2m()
{
    std::cout << "Test of mm2m" << std::endl;
    // symmetry groups
    const seque<seque<int> > emptysymm{{}};
    const seque<seque<int> > e2symm{{0, 1}, {1, 0}};
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

    struct isanelement
    {
    };
    struct node
    {
    };
    struct point
    {
    };
    struct edge
    {
    };
    struct tri
    {
    };
    struct quad
    {
    };
    struct tet
    {
    };
    struct wedge
    {
    };
    struct hex
    {
    };

    using ElementTypes = typseque<isanelement, node, point, edge, tri, quad, tet, wedge, hex>;
    // using OtherTypes = typseque<node, isanelement>;
    using MeshType = ElementTypes;
    zoo<MeshType> garden;
    garden.definesymmetrygroup<isanelement, isanelement>(emptysymm);
    garden.definesymmetrygroup<node, node>(emptysymm);
    garden.definesymmetrygroup<point, node>({{0}});
    garden.definesymmetrygroup<point, isanelement>({{0}});
    garden.definesymmetrygroup<edge, node>({{0, 1}});
    garden.definesymmetrygroup<edge, isanelement>({{0}});
    garden.definesymmetrygroup<tri, node>(t3sym);
    garden.definesymmetrygroup<tri, isanelement>({{0}});
    garden.definesymmetrygroup<quad, node>(q4sym);
    garden.definesymmetrygroup<quad, isanelement>({{0}});
    garden.definesymmetrygroup<tet, node>(tet4sym);
    garden.definesymmetrygroup<tet, isanelement>({{}});
    garden.definesymmetrygroup<wedge, node>(wedge6);
    garden.definesymmetrygroup<wedge, isanelement>({{}});
    garden.definesymmetrygroup<hex, node>(h8symm);
    garden.definesymmetrygroup<hex, isanelement>({{}});
    garden.definechildrenbuilder<tri, edge, node>({{0, 1}});
    garden.definechildrenbuilder<tri, edge, node>({{1, 2}});
    garden.definechildrenbuilder<tri, edge, node>({{2, 0}});
    garden.definechildrenbuilder<quad, edge, node>({{0, 1}});
    garden.definechildrenbuilder<quad, edge, node>({{1, 2}});
    garden.definechildrenbuilder<quad, edge, node>({{2, 3}});
    garden.definechildrenbuilder<quad, edge, node>({{3, 0}});
    garden.definechildrenbuilder<tet, tri, node>({{0, 1, 3}});
    garden.definechildrenbuilder<tet, tri, node>({{0, 3, 2}});
    garden.definechildrenbuilder<tet, tri, node>({{1, 0, 2}});
    garden.definechildrenbuilder<tet, tri, node>({{1, 2, 3}});
    garden.definechildrenbuilder<wedge, quad, node>({{1, 4, 5, 2}});
    garden.definechildrenbuilder<wedge, quad, node>({{1, 0, 3, 4}});
    garden.definechildrenbuilder<wedge, quad, node>({{0, 2, 5, 3}});
    garden.definechildrenbuilder<wedge, tri, node>({{0, 1, 2}});
    garden.definechildrenbuilder<wedge, tri, node>({{4, 3, 5}});
    garden.definechildrenbuilder<hex, quad, node>({{0, 1, 5, 4}});
    garden.definechildrenbuilder<hex, quad, node>({{1, 2, 6, 5}});
    garden.definechildrenbuilder<hex, quad, node>({{2, 3, 7, 6}});
    garden.definechildrenbuilder<hex, quad, node>({{3, 0, 4, 7}});
    garden.definechildrenbuilder<hex, quad, node>({{4, 5, 6, 7}});
    garden.definechildrenbuilder<hex, quad, node>({{1, 0, 3, 2}});
    thing p0;
    garden.appendnodestoathing<point, node>(p0, {5});
    garden.appendnodestoathing<point, isanelement>(p0, {1});
    garden.uploadelement(p0);
    thing e1;
    garden.appendnodestoathing<edge, node>(e1, {6, 5});
    garden.appendnodestoathing<edge, isanelement>(e1, {1});
    garden.uploadelement(e1);
    thing t0;
    garden.appendnodestoathing<tri, node>(t0, {4, 3, 1});
    garden.appendnodestoathing<tri, isanelement>(t0, {1});
    garden.uploadelement(t0);
    thing q0;
    garden.appendnodestoathing<quad, node>(q0, {7, 4, 3, 6});
    garden.appendnodestoathing<quad, isanelement>(q0, {1});
    garden.uploadelement(q0);
    thing q1;
    garden.appendnodestoathing<quad, node>(q1, {3, 2, 0, 1});
    garden.appendnodestoathing<quad, isanelement>(q1, {1});
    garden.uploadelement(q1);
    thing tet0;
    garden.appendnodestoathing<tet, node>(tet0, {14, 18, 15, 17});
    garden.appendnodestoathing<tet, isanelement>(tet0, {0});
    garden.uploadelement(tet0);
    thing wedge0;
    garden.appendnodestoathing<wedge, node>(wedge0, {13, 16, 10, 8, 11, 6});
    garden.appendnodestoathing<wedge, isanelement>(wedge0, {0});
    garden.uploadelement(wedge0);
    thing hex0;
    garden.appendnodestoathing<hex, node>(hex0, {7, 9, 14, 12, 6, 8, 13, 10});
    garden.appendnodestoathing<hex, isanelement>(hex0, {0});
    garden.uploadelement(hex0);
    mm2m matrix = garden.lifemanager;
    mm2m matrix2 = garden.synthesized;
    //uploadallstuff(matrix, accessories, things, models);
    compress(matrix);
    compress(matrix2);
    std::ofstream ofs;
    fileopenoutput(ofs, "matrix.txt");
    ofs << matrix;
    ofs.close();
    fileopenoutput(ofs, "matrix2.txt");
    ofs << matrix2;
    ofs.close();
    std::cout << "Matrix 2 edge node=";
    std::cout << matrix2(3, 1).nfrome << std::endl;
    ensightfromdb(matrix, matrix2);
}

#endif // TESTMMM_HPP
