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
    thing nodes[19], points[1], edges[1], tris[1], quads[2], tets[1], hexs[1],
            wedges[1];
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
    settypenumber(isanelements[0], 0);
    settypenumber(isanelements[1], 1);
    for (thing &anode: nodes)
    {
        settypenumber(anode, 2);
    }
    settypenumber(points[0], 3);
    settypenumber(edges[0], 4);
    settypenumber(tris[0], 5);
    settypenumber(quads[0], 6);
    settypenumber(quads[1], 6);
    settypenumber(tets[0], 7);
    settypenumber(wedges[0], 8);
    settypenumber(hexs[0], 9);
    // symmetry groups
    mm2m rm;
    setnumberoftypes(rm, 10);
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
    setsymmetrygroup(rm, wedge, node, wedge6);
    setsymmetrygroup(rm, wedge, isanelement, {{0}});
    setsymmetrygroup(rm, hex, node, h8symm);
    setsymmetrygroup(rm, hex, isanelement, {{0}});
    cout << "Symmetry groups=" << endl;
    cout << rm.groups << endl;

    // specialization of things
    appendnodesofonetype(isanelements[0], isanelement, {0});
    appendnodesofonetype(isanelements[1], isanelement, {1});
    for (int i=0;i<19;++i)
    {
        appendnodesofonetype(nodes[i], node, {i});
    }
    appendnodesofonetype(points[0], node, {0});
    appendnodesofonetype(points[0], isanelement, {1});
    appendnodesofonetype(edges[0], node, {7, 6});
    appendnodesofonetype(edges[0], isanelement, {1});
    appendnodesofonetype(tris[0], node, {5, 2, 4});
    appendnodesofonetype(tris[0], isanelement, {1});

    appendnodesofonetype(quads[0], node, {8, 5, 4, 7});
    appendnodesofonetype(quads[0], isanelement, {1});
    appendnodesofonetype(quads[1], node, {2, 1, 3, 4});
    appendnodesofonetype(quads[1], isanelement, {1});

    appendnodesofonetype(tets[0], node, {0, 16, 15, 18});
    appendnodesofonetype(tets[0], isanelement, {0});
    appendnodesofonetype(hexs[0], node, {13, 8, 10, 15, 11, 7, 9, 14});
    appendnodesofonetype(hexs[0], isanelement, {0});
    appendnodesofonetype(wedges[0], node, {14, 11, 17, 9, 7, 12});
    appendnodesofonetype(wedges[0], isanelement, {0});

    for (int i = 0; i < 2; ++i)
        uploadathing(rm, isanelements[i]);

    for (int i = 0; i < 19; ++i)
        uploadathing(rm, nodes[i]);
    uploadathing(rm, points[0]);
    uploadathing(rm, edges[0]);
    uploadathing(rm, tris[0]);
    uploadathing(rm, quads[0]);
    uploadathing(rm, quads[1]);
    uploadathing(rm, tets[0]);
    uploadathing(rm, hexs[0]);
    uploadathing(rm, wedges[0]);
    // now creates stuff
    seque<thing> triangles;
    append(triangles, getthingfrombuilder(tets[0], tri, {std::make_pair<int, seque<int> >(node, {0, 1, 2})}));
    append(triangles, getthingfrombuilder(tets[0], tri, {std::make_pair<int, seque<int> >(node, {1, 2, 3})}));
    append(triangles, getthingfrombuilder(tets[0], tri, {std::make_pair<int, seque<int> >(node, {2, 0, 3})}));
    append(triangles, getthingfrombuilder(tets[0], tri, {std::make_pair<int, seque<int> >(node, {0, 1, 3})}));
    append(triangles, getthingfrombuilder(wedges[0], tri, {std::make_pair<int, seque<int> >(node, {0, 1, 2})}));
    append(triangles, getthingfrombuilder(wedges[0], tri, {std::make_pair<int, seque<int> >(node, {3, 4, 5})}));
    seque<thing> quadrilaterals;
    append(quadrilaterals, getthingfrombuilder(hexs[0], quad, {std::make_pair<int, seque<int> >(node, {0, 1, 2, 3})}));
    append(quadrilaterals, getthingfrombuilder(hexs[0], quad, {std::make_pair<int, seque<int> >(node, {7, 4, 5, 6})}));
    append(quadrilaterals, getthingfrombuilder(hexs[0], quad, {std::make_pair<int, seque<int> >(node, {0, 3, 6, 7})}));
    append(quadrilaterals, getthingfrombuilder(hexs[0], quad, {std::make_pair<int, seque<int> >(node, {1, 4, 6, 2})}));
    append(quadrilaterals, getthingfrombuilder(hexs[0], quad, {std::make_pair<int, seque<int> >(node, {3, 2, 5, 6})}));
    append(quadrilaterals, getthingfrombuilder(hexs[0], quad, {std::make_pair<int, seque<int> >(node, {0, 1, 4, 7})}));
    append(quadrilaterals, getthingfrombuilder(wedges[0], quad, {std::make_pair<int, seque<int> >(node, {1, 4, 5, 2})}));
    append(quadrilaterals, getthingfrombuilder(wedges[0], quad, {std::make_pair<int, seque<int> >(node, {0, 1, 4, 3})}));
    append(quadrilaterals, getthingfrombuilder(wedges[0], quad, {std::make_pair<int, seque<int> >(node, {0, 2, 5, 3})}));
    mm2m acessories;
    uploadthings(acessories, triangles);
    uploadthings(acessories, quadrilaterals);
    auto qqq = acessories(quad, node);
}
#endif // TESTMMM_HPP
