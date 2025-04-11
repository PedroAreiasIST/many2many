//
// Created by pedro on 06-04-2025.
//

#ifndef TESTEO2M_HPP
#define TESTEO2M_HPP
#include "m2m.hpp"
#include <array>
#include <boost/container/flat_map.hpp>
#include <chrono>
#include <cstddef>
#include <random>

std::array<int, 8> generateRandomArray(int nmax) {
  std::array<int, 8> arr;

  // Create a random number generator seeded with a random_device.
  std::random_device rd;
  std::mt19937 gen(rd());

  // Ensure nmax is large enough to allow unique values.
  if (nmax < arr.size()) {
    throw std::invalid_argument(
        "nmax must be at least 8 to generate unique numbers.");
  }

  // Create a vector containing all possible values and shuffle it.
  std::vector<int> values(nmax);
  std::iota(values.begin(), values.end(),
            0); // Fill values with 0, 1, ..., nmax-1
  std::shuffle(values.begin(), values.end(), gen);
  // Copy the first 8 unique values to the array.
  std::copy_n(values.begin(), arr.size(), arr.begin());
  return arr;
}

inline void testeo2m() {
  int nel, nmax;
  seque<int> els;
  m2m mm;
  if constexpr (2 == 2) {
    constexpr int ntype = 6;
    int e = 0;
    switch (ntype) {
    case 6:
      nel = 400;
      nmax = pow(nel, 3);
      setsize(els, nmax);
      std::cout << "Started" << std::endl;
      setnumberofelements(mm, nmax);

      for (int iex = 0; iex < nel; ++iex)
        for (int iey = 0; iey < nel; ++iey)
          for (int iez = 0; iez < nel; ++iez) {
            seque<int> nodes(8);
            nodes[0] = iex + iey * (nel + 1) + iez * pow(nel + 1, 2);
            nodes[1] = (iex + 1) + iey * (nel + 1) + iez * pow(nel + 1, 2);
            nodes[2] =
                (iex + 1) + (iey + 1) * (nel + 1) + iez * pow(nel + 1, 2);
            nodes[3] = iex + (iey + 1) * (nel + 1) + iez * pow(nel + 1, 2);
            nodes[4] = iex + iey * (nel + 1) + (iez + 1) * pow(nel + 1, 2);
            nodes[5] =
                (iex + 1) + iey * (nel + 1) + (iez + 1) * pow(nel + 1, 2);
            nodes[6] =
                (iex + 1) + (iey + 1) * (nel + 1) + (iez + 1) * pow(nel + 1, 2);
            nodes[7] =
                iex + (iey + 1) * (nel + 1) + (iez + 1) * pow(nel + 1, 2);
            // appendelement(mm, nodes);
            setnodesforelement(mm, e++, nodes);
          }
      break;
    case 4:
      nel = 12000;
      nmax = pow(nel, 2);
      setsize(els, nmax);
      std::cout << "Started" << std::endl;
      setnumberofelements(mm, nmax);
      std::cout << "Started" << std::endl;
      setnumberofelements(mm, nmax);
      for (int iex = 0; iex < nel; ++iex)
        for (int iey = 0; iey < nel; ++iey) {
          seque<int> nodes(4);
          nodes[0] = iex + iey * (nel + 1);
          nodes[1] = (iex + 1) + iey * (nel + 1);
          nodes[2] = (iex + 1) + (iey + 1) * (nel + 1);
          nodes[3] = iex + (iey + 1) * (nel + 1);
          setnodesforelement(mm, e++, nodes);
        }
      break;
    default:
      break;
    }

    // o2m one = mm.nodesfromelement;
    // std::cout << "e=" << e << std::endl;
    // std::cout << "Finished inserting stuff" << std::endl;
    // std::cout << "How many ?" << mm.nodesfromelement.nelem << std::endl;
    // setallpointers(mm);
    o2m &om1 = mm.nodesfromelement;
    // std::cout << "Finished setting the pointers" << std::endl;
    m2m result;
    auto start_time = std::chrono::high_resolution_clock::now();
    std::cout << "tr beg\n";
    o2m om2 = transpose(om1);
    std::cout << "tr end\n";
    o2m om3;
    std::cout << "mult beg\n";
    multiplication(om1, om2, om3);
    std::cout << "mult end\n";
    // getnodestonodes(mm, result);
    //   std::cout << "Finished the transpose stuff" << std::endl;
    //   auto start_time = std::chrono::high_resolution_clock::now();
    // o2m two = transpose(one);
    //  o2m cad = getcliqueaddressing(one, two);
    //  std::cout << "Transposed" << std::endl;
    //  auto resultado = two * seque<int>({1, 2, 3, 4});
    //  o2m three = one * two;
    std::cout << result.nodesfromelement[0] << std::endl;
    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(
        end_time - start_time);
    std::cout << "Duration: " << duration.count() << " milliseconds"
              << std::endl;
    std::cout << result.nodesfromelement.lnods[0] << std::endl;
  }
}
#endif // TESTEO2M_HPP
