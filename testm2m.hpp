//
// Created by pedro on 4/3/25.
//

#ifndef TESTMM_HPP
#define TESTMM_HPP
#include "basics.hpp"
#include "m2m.hpp"
using namespace std;
void testm2m()
{
    std::vector<std::string> nomestodos;
    PFRALLNAMES(m2m, nomestodos);
    for (int i = 0; i < nomestodos.size(); ++i)
        cout << nomestodos[i] << endl;
    // conectivities
    m2m mm1, mm, nn1, ee1;
    appendelement(mm1, {4, 2, 6, 0});
    appendelement(mm1, {3, 0});
    appendelement(mm1, {3, 1});
    appendelement(mm1, {6, 3, 5, 1, 2, 4});
    setallpointers(mm1);
    cout << "Basics mm part 1" << endl;
    cout << "mm1=" << mm1 << endl;
    cout << "How about maxnodenumber" << mm1.nodesfromelement.maxnodenumber << endl;
    cout << "elements defined by the 3 1 nodes" << getelementsfromnodes(mm1, {3, 1}) << endl;
    cout << "elements defined by the 3 0 nodes" << getelementsfromnodes(mm1, {3, 0}) << endl;
    cout << "elements containing the 2 6 nodes" << getelementswithnodes(mm1, {2, 6}) << endl;
    cout << "local node 5 position for 0th element=" << getlocalnodeposition(mm1, 5, 0) << endl;
    cout << "local node 3 position for 1th element=" << getlocalnodeposition(mm1, 3, 1) << endl;
    cout << "neighbours of 0=" << getelementneighbours(mm1, 0) << endl;
    cout << "neighbours of 2=" << getelementneighbours(mm1, 2) << endl;
    cout << "lexicographical order of mm1=" << mm1.nodesfromelement.lnods(lexiorder(mm1)) << endl;
    getnodestonodes(mm1, nn1);
    cout << "nodestonodes=" << nn1.elementsfromnode.lnods << endl;
    getelementstoelements(mm1, ee1);
    cout << "elementstoelements=" << ee1.elementsfromnode.lnods << endl;
    cout << "Basics mm part 2" << endl;
    m2m mm2, mm3;
    appendelement(mm2, {4, 9, 8, 0, 1});
    appendelement(mm2, {3, 2, 0, 5});
    appendelement(mm2, {3, 1, 4});
    addition(mm1, false, mm2, false, mm3);
    cout << "mm1+mm2=" << mm3.nodesfromelement.lnods << endl;
    addition(mm2, false, mm1, false, mm3);
    cout << "mm2+mm1=" << mm3.nodesfromelement.lnods << endl;
    multiplication(mm1, false, mm2, false, mm3);
    cout << "mm1*mm2=" << mm3.nodesfromelement.lnods << endl;
    multiplication(mm2, false, mm1, false, mm3);
    cout << "mm2*mm1=" << mm3.nodesfromelement.lnods << endl;
    subtraction(mm1, false, mm2, false, mm3);
    cout << "mm1-mm2=" << mm3.nodesfromelement.lnods << endl;
    subtraction(mm2, false, mm1, false, mm3);
    cout << "mm2-mm1=" << mm3.nodesfromelement.lnods << endl;
    intersection(mm1, false, mm2, false, mm3);
    cout << "mm1&mm2=" << mm3.nodesfromelement.lnods << endl;
    intersection(mm2, false, mm1, false, mm3);
    cout << "mm2&mm1=" << mm3.nodesfromelement.lnods << endl;
    seque<size_t> se{0, 1, 3};
    cout << "Before compression=" << mm1.nodesfromelement.lnods << endl;
    compresselements(mm1, se);
    cout << "After compression=" << mm1.nodesfromelement.lnods << endl;
    seque<size_t> sn{0, 2, 1, 4, 3, 5, 7, 9, 8};
    permutenodes(mm1, sn);
    cout << "After node compression" << mm1.nodesfromelement.lnods << endl;
}
#endif // TESTMM_HPP
