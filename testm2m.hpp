//
// Created by pedro on 4/3/25.
//

#ifndef TESTMM_HPP
#define TESTMM_HPP
#include "basics.hpp"
#include "m2m.hpp"
using namespace std;

void testm2m() {
  std::vector<std::string> nomestodos;
  PFRALLNAMES(m2m, nomestodos);
  for (int i = 0; i < nomestodos.size(); ++i)
    cout << nomestodos[i] << endl;
  // conectivities
  m2m mm1, mm, nn1, ee1;
  appendelement(mm1.nfrome, {4, 2, 6, 0});
  appendelement(mm1.nfrome, {3, 0});
  appendelement(mm1.nfrome, {3, 1});
  appendelement(mm1.nfrome, {6, 3, 5, 1, 2, 4});
  cout << "Appended above" << endl;
  setsyncronized(mm1);
  cout << "Cliques" << endl;
  cout << "nodefromelem" << mm1.nfrome.lnods << endl;
  cout << "elementfromnode" << mm1.efromn.lnods << endl;
  auto locations = hidden::getnodepositions(mm1.nfrome, mm1.efromn);
  auto locations2 = hidden::getelementpositions(mm1.nfrome, mm1.efromn);
  auto cliques = getcliques(mm1.nfrome, mm1.efromn);
  cout << "locations" << locations << endl;
  cout << "locations2" << locations2 << endl;
  cout << "cliques" << endl;
  cout << cliques << endl;
  cout << "Basics mm part 1" << endl;
  cout << "mm1=" << mm1 << endl;
  cout << "How about maxnode" << mm1.nfrome.maxnode << endl;
  cout << "elements defined by the 3 1 nodes"
       << getelementsfromnodes(mm1, {3, 1}) << endl;
  cout << "elements defined by the 3 0 nodes"
       << getelementsfromnodes(mm1, {3, 0}) << endl;
  cout << "elements containing the 2 6 nodes"
       << getelementswithnodes(mm1, {2, 6}) << endl;
  cout << "neighbours of element 0=" << getelementneighbours(mm1, 0) << endl;
  cout << "neighbours of element 1=" << getelementneighbours(mm1, 1) << endl;
  cout << "neighbours of node 3=" << getnodeneighbours(mm1, 3) << endl;
  cout << "lexicographical order of mm1=" << mm1.nfrome.lnods(getorder(mm1))
       << endl;
  ee1 = getelementstoelements(mm1);
  nn1 = getnodestonodes(mm1);
  cout << "nodestonodes=" << nn1.nfrome.lnods << endl;
  cout << "elementstoelements=" << ee1.nfrome.lnods << endl;

  cout << "Basics mm part 2" << endl;
  m2m mm2, mm3;
  auto a = {4, 9, 8, 0, 1};
  auto b = {3, 2, 0, 5};
  auto c = {3, 1, 4};
  mm2.nfrome << a;
  mm2.nfrome << b;
  mm2.nfrome << c;
  mm3.nfrome = mm1.nfrome + mm2.nfrome;
  cout << "mm1+mm2=" << mm3.nfrome.lnods << endl;
  mm3.nfrome = mm1.nfrome * mm2.nfrome;
  cout << "mm1*mm2=" << mm3.nfrome.lnods << endl;
  mm3.nfrome = mm2.nfrome * mm1.nfrome;
  cout << "mm2*mm1=" << mm3.nfrome.lnods << endl;
  mm3.nfrome = mm1.nfrome - mm2.nfrome;
  cout << "mm1-mm2=" << mm3.nfrome.lnods << endl;
  mm3.nfrome = mm1.nfrome && mm2.nfrome;
  cout << "mm1&mm2=" << mm3.nfrome.lnods << endl;
  mm3.nfrome = mm2.nfrome && mm1.nfrome;
  cout << "mm2&mm1=" << mm3.nfrome.lnods << endl;
  seque<int> se{0, 1, 3};
  cout << "Before compression=" << mm1.nfrome.lnods << endl;
  compresselements(mm1, se);
  cout << "After compression=" << mm1.nfrome.lnods << endl;
  seque<int> sn{0, 2, 1, 4, 3, 5, 7, 9, 8};
  cout << "Permute beg" << endl;
  permutenodes(mm1, sn);
  cout << "Permute end" << endl;
  cout << "After node compression" << mm1.nfrome.lnods << endl;
}
#endif // TESTMM_HPP
