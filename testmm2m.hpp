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
    // thing skeletons
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
        wedge,
        hex
    };
    seque<thingmodel> models;
    appendsymmetrygroup(models, isanelement, isanelement, emptysymm);
    appendsymmetrygroup(models, node, node, emptysymm);
    appendsymmetrygroup(models, point, node, emptysymm);
    appendsymmetrygroup(models, point, isanelement, emptysymm);
    appendsymmetrygroup(models, edge, node, e2symm);
    appendsymmetrygroup(models, edge, isanelement, emptysymm);
    appendsymmetrygroup(models, tri, node, t3sym);
    appendsymmetrygroup(models, tri, isanelement, emptysymm);
    appendsymmetrygroup(models, quad, node, q4sym);
    appendsymmetrygroup(models, quad, isanelement, emptysymm);
    appendsymmetrygroup(models, tet, node, tet4sym);
    appendsymmetrygroup(models, tet, isanelement, emptysymm);
    appendsymmetrygroup(models, wedge, node, wedge6);
    appendsymmetrygroup(models, wedge, isanelement, emptysymm);
    appendsymmetrygroup(models, hex, node, tet4sym);
    appendsymmetrygroup(models, hex, isanelement, emptysymm);
    appendchildrenbuilder(models, tri, edge, node, {0, 1});
    appendchildrenbuilder(models, tri, edge, node, {1, 2});
    appendchildrenbuilder(models, tri, edge, node, {2, 0});
    appendchildrenbuilder(models, quad, edge, node, {0, 1});
    appendchildrenbuilder(models, quad, edge, node, {1, 2});
    appendchildrenbuilder(models, quad, edge, node, {2, 3});
    appendchildrenbuilder(models, quad, edge, node, {3, 0});
    appendchildrenbuilder(models, tet, tri, node, {0, 1, 3});
    appendchildrenbuilder(models, tet, tri, node, {0, 3, 2});
    appendchildrenbuilder(models, tet, tri, node, {1, 0, 2});
    appendchildrenbuilder(models, tet, tri, node, {1, 2, 3});
    appendchildrenbuilder(models, wedge, quad, node, {1, 4, 5, 2});
    appendchildrenbuilder(models, wedge, quad, node, {1, 0, 3, 4});
    appendchildrenbuilder(models, wedge, quad, node, {0, 2, 5, 3});
    appendchildrenbuilder(models, wedge, tri, node, {0, 1, 2});
    appendchildrenbuilder(models, wedge, tri, node, {4, 3, 5});
    appendchildrenbuilder(models, hex, quad, node, {0, 1, 5, 4});
    appendchildrenbuilder(models, hex, quad, node, {1, 2, 6, 5});
    appendchildrenbuilder(models, hex, quad, node, {2, 3, 7, 6});
    appendchildrenbuilder(models, hex, quad, node, {3, 0, 4, 7});
    appendchildrenbuilder(models, hex, quad, node, {4, 5, 6, 7});
    appendchildrenbuilder(models, hex, quad, node, {1, 0, 3, 2});
    seque<thing> things{29};
    settypenumberofathing(things[0], isanelement);
    settypenumberofathing(things[1], isanelement);
    for (int i = 0; i < 19; ++i)
        settypenumberofathing(things[i + 2], node);
    settypenumberofathing(things[21], point);
    settypenumberofathing(things[22], edge);
    settypenumberofathing(things[23], tri);
    settypenumberofathing(things[24], quad);
    settypenumberofathing(things[25], quad);
    settypenumberofathing(things[26], tet);
    settypenumberofathing(things[27], wedge);
    settypenumberofathing(things[28], hex);
    appendnodesofonetype(things[21], node, {5});
    appendnodesofonetype(things[21], isanelement, {1});
    appendnodesofonetype(things[22], node, {6, 5});
    appendnodesofonetype(things[22], isanelement, {1});
    appendnodesofonetype(things[23], node, {4, 3, 1});
    appendnodesofonetype(things[23], isanelement, {1});
    appendnodesofonetype(things[24], node, {7, 4, 3, 6});
    appendnodesofonetype(things[24], isanelement, {1});
    appendnodesofonetype(things[25], node, {3, 2, 0, 1});
    appendnodesofonetype(things[25], isanelement, {1});
    appendnodesofonetype(things[26], node, {14, 18, 15, 17});
    appendnodesofonetype(things[26], isanelement, {0});
    appendnodesofonetype(things[27], node, {13, 16, 10, 8, 11, 6});
    appendnodesofonetype(things[27], isanelement, {0});
    appendnodesofonetype(things[28], node, {7, 9, 14, 12, 6, 8, 13, 10});
    appendnodesofonetype(things[28], isanelement, {0});
    mm2m matrix;
    setnumberoftypes(matrix, 9);
    mm2m accessories;
    setnumberoftypes(accessories, 9);
    uploadallstuff(matrix, accessories, things, models);
    std::ofstream ofs;
    fileopenoutput(ofs, "matrix.txt");
    setcompressed(matrix);
    ofs << "hexahedra\n";
    ofs << "hex,node" << matrix(hex, node).nfrome << "\n";
    ofs << "node,hex" << matrix(hex, node).efromn << "\n";
    ofs << "hex,isanelement" << matrix(hex, isanelement).nfrome << "\n";
    ofs << "isanelement,quad" << matrix(isanelement, quad).nfrome << "\n";
    ofs << "isanalement hex--- efromn" << matrix(hex, isanelement).efromn
            << std::endl;
    ofs << "isanalement quad--- efromn" << matrix(quad, isanelement).efromn
            << std::endl;
    ofs << "Full matrix" << matrix << endl;
    ofs << "All the nodes of a wedge\n";
    ofs << getallnodes(matrix, wedge, 0) << "\n";
    ofs << "all relations to isanelement 0:"
            << getallelements(matrix, isanelement, 0) << "\n";
    ofs << "all relations to isanelement 1:"
            << getallelements(matrix, isanelement, 1) << "\n";
    ensightfromdb(matrix);
}

#endif // TESTMMM_HPP
