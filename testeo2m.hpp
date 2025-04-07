//
// Created by pedro on 06-04-2025.
//

#ifndef TESTEO2M_HPP
#define TESTEO2M_HPP
#include <array>
#include <random>
#include <cstddef>
#include "m2m.hpp"
std::array<std::size_t, 8> generateRandomArray(std::size_t nmax) {
    std::array<std::size_t, 8> arr;

    // Create a random number generator seeded with a random_device.
    std::random_device rd;
    std::mt19937 gen(rd());

    // Create a uniform distribution from 0 to nmax.
    std::uniform_int_distribution<std::size_t> dis(0, nmax);

    // Fill the array with random numbers.
    for (auto &element : arr) {
        element = dis(gen);
    }

    return arr;
}
inline void testeo2m()
{
    size_t nmax=100000000;
    seque<size_t> els(nmax+1);
    std::array<size_t,8> arr;
    m2m mm;
    std::cout<<"Started"<<std::endl;
    setnumberofelements(mm, nmax+1);
    for (size_t e=0;e<nmax;++e) {
        arr=generateRandomArray(nmax);
        seque<size_t> nodes(arr.size());
        std::copy(arr.begin(),arr.end(),nodes.begin());
        setnodesforelement(mm,e,nodes);
    }
    std::cout<<"Finished inserting stuff"<<std::endl;
    setallpointers(mm);
    std::cout<<"Finished setting the pointers"<<std::endl;
    m2m result;
    getnodestonodes(mm, result);
    std::cout<<"Finished the transpose stuff"<<std::endl;
    std::cout<<result.nodesfromelement.lnods[0]<<std::endl;
}
#endif //TESTEO2M_HPP
