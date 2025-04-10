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

std::array<std::size_t, 8> generateRandomArray(std::size_t nmax) {
  std::array<std::size_t, 8> arr;

  // Create a random number generator seeded with a random_device.
  std::random_device rd;
  std::mt19937 gen(rd());

  // Ensure nmax is large enough to allow unique values.
  if (nmax < arr.size()) {
    throw std::invalid_argument(
        "nmax must be at least 8 to generate unique numbers.");
  }

  // Create a vector containing all possible values and shuffle it.
  std::vector<std::size_t> values(nmax);
  std::iota(values.begin(), values.end(),
            0); // Fill values with 0, 1, ..., nmax-1
  std::shuffle(values.begin(), values.end(), gen);

  // Copy the first 8 unique values to the array.
  std::copy_n(values.begin(), arr.size(), arr.begin());

  return arr;
}

inline void testeo2m() {
  size_t nel, nmax;
  seque<size_t> els;
  m2m mm;
  if (2 == 2) {

    constexpr size_t ntype = 6;

    size_t e = 0;
    switch (ntype) {
    case 6:
      nel = 400;
      nmax = pow(nel, 3);
      setsize(els, nmax);
      std::cout << "Started" << std::endl;
      setnumberofelements(mm, nmax);

      for (size_t iex = 0; iex < nel; ++iex)
        for (size_t iey = 0; iey < nel; ++iey)
          for (size_t iez = 0; iez < nel; ++iez) {
            seque<size_t> nodes(8);
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
      for (size_t iex = 0; iex < nel; ++iex)
        for (size_t iey = 0; iey < nel; ++iey) {
          seque<size_t> nodes(4);
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

    o2m one = mm.nodesfromelement;

    std::cout << "e=" << e << std::endl;
    std::cout << "Finished inserting stuff" << std::endl;
    std::cout << "How many ?" << mm.nodesfromelement.nelem << std::endl;

    // setallpointers(mm);
    // std::cout << "Finished setting the pointers" << std::endl;
    m2m result;
    // getnodestonodes(mm, result);
    //  std::cout << "Finished the transpose stuff" << std::endl;
    auto start_time = std::chrono::high_resolution_clock::now();
    o2m two = transpose(one);
    std::cout << "Transposed" << std::endl;
    // auto resultado = two * seque<size_t>({1, 2, 3, 4});
    o2m three = one * two;
    std::cout << three.lnods[0] << std::endl;
    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(
        end_time - start_time);
    std::cout << "Duration: " << duration.count() << " milliseconds"
              << std::endl;
    std::cout << result.nodesfromelement.lnods[0] << std::endl;
  } else {
    nel = 100;
    nmax = pow(nel, 3);
    setsize(els, nmax);
    seque<size_t> fixenode(8);
    std::cout << "Started" << std::endl;
    setnumberofelements(mm, nmax);
    size_t e = 0;
#ifdef _OPENMP
#pragma omp parallel for collapse(3)
#endif

    for (size_t iex = 0; iex < nel; ++iex)
      for (size_t iey = 0; iey < nel; ++iey)
        for (size_t iez = 0; iez < nel; ++iez) {
          seque<size_t> nodes(8);
          nodes[0] = iex + iey * (nel + 1) + iez * pow(nel + 1, 2);
          nodes[1] = (iex + 1) + iey * (nel + 1) + iez * pow(nel + 1, 2);
          nodes[2] = (iex + 1) + (iey + 1) * (nel + 1) + iez * pow(nel + 1, 2);
          nodes[3] = iex + (iey + 1) * (nel + 1) + iez * pow(nel + 1, 2);
          nodes[4] = iex + iey * (nel + 1) + (iez + 1) * pow(nel + 1, 2);
          nodes[5] = (iex + 1) + iey * (nel + 1) + (iez + 1) * pow(nel + 1, 2);
          nodes[6] =
              (iex + 1) + (iey + 1) * (nel + 1) + (iez + 1) * pow(nel + 1, 2);
          nodes[7] = iex + (iey + 1) * (nel + 1) + (iez + 1) * pow(nel + 1, 2);
          if (e == 1302)
#ifdef _OPENMP
#pragma omp critical
#endif
            fixenode = nodes;
#ifdef _OPENMP
#pragma omp critical
#endif

          setnodesforelement(mm, e++, nodes);
        }

    constexpr bool HASH(true);
    setallpointers(mm);

    seque<size_t> order = lexiorder(mm);
    std::cout << mm.nodesfromelement.lnods[order[0]] << std::endl;
    std::cout << mm.nodesfromelement.lnods[order[1]] << std::endl;
    std::cout << mm.nodesfromelement.lnods[order[2]] << std::endl;

    seque<size_t> oldfromnew, newfromold;
    indicesfromorder(mm.nodesfromelement, order, oldfromnew, newfromold);
    auto start_time2 = std::chrono::high_resolution_clock::now();
    compresselements(mm, oldfromnew);
    permutenodes(mm, newfromold);
    auto end_time2 = std::chrono::high_resolution_clock::now();
    auto duration2 = std::chrono::duration_cast<std::chrono::milliseconds>(
        end_time2 - start_time2);
    std::cout << "Elapsed time for indices from ordder: " << duration2.count()
              << " milliseconds" << std::endl;

    return;

    auto start_time = std::chrono::high_resolution_clock::now();
    if (!HASH) {
      seque<size_t> obtained = getelementsfromnodes(mm, fixenode);
      std::cout << obtained << std::endl;
    } else {
      std::map<seque<size_t>, size_t> vas;
      // vas.reserve(mm.nodesfromelement.nelem);
      for (size_t i = 0; i < mm.nodesfromelement.nelem; ++i) {
        vas[mm.nodesfromelement.lnods[i]] = i;
      }
      size_t obtained = vas.find(fixenode)->second;
      std::cout << obtained << std::endl;
    }
    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(
        end_time - start_time);
    std::cout << "Duration: " << duration.count() << " milliseconds"
              << std::endl;
  }
}
#endif // TESTEO2M_HPP
