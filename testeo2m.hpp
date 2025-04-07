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
    size_t nel=100;
    size_t nmax=pow(nel,3);
    seque<size_t> els(nmax);
    m2m mm;
    std::cout<<"Started"<<std::endl;
    setnumberofelements(mm,nmax);
    size_t e=0;
    for (size_t iex=0;iex<nel;++iex)
        for (size_t iey=0;iey<nel;++iey)
            for (size_t iez=0;iez<nel;++iez) {
                e++;
                seque<size_t> nodes(8);
                nodes[1]=iex+iey*(nel+1)+iez*pow(nel+1,2);
                nodes[2]=(iex+1)+iey*(nel+1)+iez*pow(nel+1,2);
                nodes[3]=(iex+1)+(iey+1)*(nel+1)+iez*pow(nel+1,2);
                nodes[4]=iex+(iey+1)*(nel+1)+iez*pow(nel+1,2);
                nodes[5]=iex+iey*(nel+1)+(iez+1)*pow(nel+1,2);
                nodes[6]=(iex+1)+iey*(nel+1)+(iez+1)*pow(nel+1,2);
                nodes[7]=(iex+1)+(iey+1)*(nel+1)+(iez+1)*pow(nel+1,2);
                nodes[8]=iex+(iey+1)*(nel+1)+(iez+1)*pow(nel+1,2);
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
