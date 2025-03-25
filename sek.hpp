#ifndef SEK_HPP
#define SEK_HPP

#include <algorithm>
#include <cassert>
#include <cmath>
#include <execution>
#include <initializer_list>
#include <iostream>
#include <numeric>
#include <omp.h>
#include <random>
#include "basics.hpp"

namespace detail
{
    static constexpr size_t STACKSIZE = 10;
}
using detail::STACKSIZE;

template<typename V, size_t S = STACKSIZE, auto P = std::execution::par>
struct sek
{
    static constexpr size_t stacksize = S;
    static constexpr auto parallel = P;
    size_t size = 0;
    V stackdata[S];
    V *heapdata = nullptr;
    size_t heapsize = 0;
    V *actual = stackdata;

    sek() { std::fill(stackdata, stackdata + stacksize, V()); }
    explicit sek(size_t newSize) : sek() { setsize(*this, newSize); }
    sek(size_t newSize, V value) : sek(newSize) { std::fill(actual, actual + size, value); }
    sek(std::initializer_list<V> initList) : sek(initList.size())
    {
        std::move(initList.begin(), initList.end(), actual);
    }
    sek(sek const &other) : sek()
    {
        setsize(*this, other.size);
        std::copy(parallel, other.actual, other.actual + other.size, actual);
    }
    sek(sek &&other) noexcept : sek()
    {
        size = other.size;
        if (other.actual == other.stackdata)
        {
            actual = stackdata;
            std::move(other.stackdata, other.stackdata + other.size, stackdata);
        } else
        {
            heapdata = other.heapdata;
            actual = heapdata;
            heapsize = other.heapsize;
            other.heapdata = nullptr;
            other.heapsize = 0;
        }
        other.size = 0;
    }
    ~sek() { setsize(*this, 0); }

    sek &operator=(sek const &other)
    {
        if (this != &other)
        {
            setsize(*this, other.size);
            std::copy(parallel, other.actual, other.actual + other.size, actual);
        }
        return *this;
    }
    sek &operator=(sek &&other) noexcept
    {
        if (this != &other)
        {
            if (heapdata && actual == heapdata)
            {
                arraydestroy(heapdata);
                heapdata = nullptr;
                heapsize = 0;
            }
            std::fill(stackdata, stackdata + stacksize, V());
            if (other.actual == other.stackdata)
            {
                actual = stackdata;
                std::move(other.stackdata, other.stackdata + other.size, stackdata);
            } else
            {
                heapdata = other.heapdata;
                actual = heapdata;
                heapsize = other.heapsize;
                other.heapdata = nullptr;
                other.heapsize = 0;
            }
            size = other.size;
            other.size = 0;
        }
        return *this;
    }
    sek &operator=(V value)
    {
        std::fill(actual, actual + size, value);
        return *this;
    }
    sek &operator=(std::initializer_list<V> initList)
    {
        setsize(*this, initList.size());
        std::move(initList.begin(), initList.end(), actual);
        return *this;
    }

    V &operator[](size_t index) { return actual[index]; }
    V const &operator[](size_t index) const { return actual[index]; }
    V &operator()(size_t index)
    {
        if (index >= size)
            setsize(*this, index + 1);
        return actual[index];
    }
    V &operator()(size_t index) const
    {
        _explodeinvalidindex(index);
        return actual[index];
    }

    V *begin() { return actual; }
    V *end() { return actual + size; }
    V const *begin() const { return actual; }
    V const *end() const { return actual + size; }

    sek operator()(sek<size_t, S, P> const &indexContainer) const
    {
        sek result(indexContainer.size);
        std::transform(indexContainer.actual, indexContainer.actual + indexContainer.size, result.actual,
                       [&](size_t idx) { return actual[idx]; });
        return result;
    }

    void _switchfromstacktoheap(size_t newCapacity)
    {
        heapsize = newCapacity;
        arraycreate(heapdata, newCapacity);
        std::move(stackdata, stackdata + size, heapdata);
        actual = heapdata;
    }
    void _switchfromheapstostack()
    {
        size_t minCapacity = std::min(heapsize, stacksize);
        if (heapdata)
        {
            std::move(heapdata, heapdata + minCapacity, stackdata);
            arraydestroy(heapdata);
            heapdata = nullptr;
        }
        std::fill(stackdata + minCapacity, stackdata + stacksize, V());
        heapsize = 0;
        actual = stackdata;
    }
    void _modifysizeofheap(size_t newCapacity)
    {
        if (newCapacity > heapsize)
        {
            V *tempArray = nullptr;
            arraycreate(tempArray, newCapacity);
            std::move(heapdata, heapdata + size, tempArray);
            arraydestroy(heapdata);
            heapdata = tempArray;
            heapsize = newCapacity;
            actual = heapdata;
        }
    }
    void _explodeinvalidindex(size_t index) const
    {
        if (index >= size)
        {
            std::cout << "Container " << typetoname<sek>() << "\nIndex " << index << " out of range.\n";
            exit(1);
        }
    }
};

template<typename V, size_t S, auto P>
inline V *begin(sek<V, S, P> &container)
{
    return container.actual;
}
template<typename V, size_t S, auto P>
inline V *end(sek<V, S, P> &container)
{
    return container.actual + container.size;
}

template<typename V, size_t S, auto P>
void setsize(sek<V, S, P> &container, size_t newSize, double growthFactor = 1.5)
{
    assert(growthFactor > 1.0);
    if (container.actual == container.stackdata)
    {
        if (newSize > container.stacksize)
            container._switchfromstacktoheap(static_cast<size_t>(std::round(growthFactor * newSize)));
    } else
    {
        if (newSize <= container.stacksize)
            container._switchfromheapstostack();
        else if (newSize > container.heapsize)
            container._modifysizeofheap(static_cast<size_t>(std::round(growthFactor * newSize)));
    }
    container.size = newSize;
}

template<typename V, size_t S, auto P>
void save(auto archiver, sek<V, S, P> const &container)
{
    archiver(container.size);
    for (size_t index = 0; index < container.size; ++index)
        archiver(container.actual[index]);
}

template<typename V, size_t S, auto P>
void load(auto archiver, sek<V, S, P> &container)
{
    size_t loadedSize = 0;
    archiver(loadedSize);
    setsize(container, loadedSize);
    for (size_t index = 0; index < container.size; ++index)
        archiver(container.actual[index]);
}

template<typename V, size_t S, auto P>
void swap(sek<V, S, P> &containerA, sek<V, S, P> &containerB)
{
    if (containerA.actual == containerA.heapdata && containerB.actual == containerB.heapdata)
    {
        std::swap(containerA.heapdata, containerB.heapdata);
        std::swap(containerA.actual, containerB.actual);
        std::swap(containerA.heapsize, containerB.heapsize);
        std::swap(containerA.size, containerB.size);
    } else
    {
        auto temp = containerA;
        containerA = containerB;
        containerB = temp;
    }
}

template<typename V, size_t S, auto P>
bool isindexvalid(sek<V, S, P> const &container, size_t index)
{
    return index < container.size;
}

template<typename V, size_t S, auto P>
void erase(sek<V, S, P> &container)
{
    setsize(container, 0);
}

template<typename V, size_t S, auto P>
void erase(sek<V, S, P> &container, size_t eraseIndex)
{
    if (isindexvalid(container, eraseIndex) && container.size > 0)
    {
        container.actual[eraseIndex] = container.actual[container.size - 1];
        container.actual[container.size - 1] = V();
        setsize(container, container.size - 1);
    }
}

template<typename V, size_t S, auto P>
void erase(sek<V, S, P> &container, sek<size_t, S, P> const &indexContainer)
{
    for (size_t pos = 0; pos < indexContainer.size; ++pos)
        erase(container, indexContainer[pos]);
}

template<typename V, size_t S, auto P>
void eraselast(sek<V, S, P> &container)
{
    if (container.size > 0)
        setsize(container, container.size - 1);
}

template<typename V, size_t S, auto P>
void eraseinplace(sek<V, S, P> &container, size_t startIndex, size_t numToErase = 1)
{
    if (!numToErase || startIndex >= container.size)
        return;
    if (startIndex + numToErase > container.size)
        numToErase = container.size - startIndex;
    std::move(container.actual + startIndex + numToErase, container.actual + container.size,
              container.actual + startIndex);
    setsize(container, container.size - numToErase);
}

template<typename V, size_t S, auto P>
void add(sek<V, S, P> &container, size_t insertIndex, V value)
{
    container._explodeinvalidindex(insertIndex);
    setsize(container, container.size + 1);
    container[container.size - 1] = container[insertIndex];
    container[insertIndex] = value;
}

template<typename V, size_t S, auto P>
void addinplace(sek<V, S, P> &container, size_t insertIndex, V value)
{
    if (insertIndex > container.size)
        insertIndex = container.size;
    setsize(container, container.size + 1);
    std::move_backward(container.actual + insertIndex, container.actual + container.size - 1,
                       container.actual + container.size);
    container[insertIndex] = value;
}

template<typename V, size_t S, auto P>
void addinplace(sek<V, S, P> &destContainer, size_t insertIndex, sek<V, S, P> const &sourceContainer)
{
    if (insertIndex > destContainer.size)
        insertIndex = destContainer.size;
    destContainer._explodeinvalidindex(insertIndex == 0 ? 0 : insertIndex - 1);
    auto oldSize = destContainer.size;
    setsize(destContainer, destContainer.size + sourceContainer.size);
    std::move_backward(destContainer.actual + insertIndex, destContainer.actual + oldSize,
                       destContainer.actual + destContainer.size);
    std::copy(destContainer.parallel, sourceContainer.actual, sourceContainer.actual + sourceContainer.size,
              destContainer.actual + insertIndex);
}

template<typename V, size_t S, auto P>
void append(sek<V, S, P> &container, V value)
{
    setsize(container, container.size + 1);
    container[container.size - 1] = value;
}

template<typename V, size_t S, auto P>
void append(sek<V, S, P> &destContainer, sek<V, S, P> const &sourceContainer)
{
    size_t oldSize = destContainer.size;
    setsize(destContainer, destContainer.size + sourceContainer.size);
    std::copy(destContainer.parallel, sourceContainer.actual, sourceContainer.actual + sourceContainer.size,
              destContainer.actual + oldSize);
}

template<typename V, size_t S, auto P>
void append(sek<V, S, P> &destContainer, sek<V, S, P> &&sourceContainer)
{
    size_t oldSize = destContainer.size;
    setsize(destContainer, destContainer.size + sourceContainer.size);
    std::move(sourceContainer.actual, sourceContainer.actual + sourceContainer.size, destContainer.actual + oldSize);
}

template<typename V, size_t S, auto P>
size_t getsize(sek<V, S, P> const &container)
{
    return container.size;
}

template<typename V, size_t S, auto P>
bool operator<(sek<V, S, P> const &lhs, sek<V, S, P> const &rhs)
{
    return std::lexicographical_compare(lhs.actual, lhs.actual + lhs.size, rhs.actual, rhs.actual + rhs.size,
                                        [](auto leftElem, auto rightElem) { return leftElem < rightElem; });
}
template<typename V, size_t S, auto P>
bool operator>(sek<V, S, P> const &lhs, sek<V, S, P> const &rhs)
{
    return rhs < lhs;
}
template<typename V, size_t S, auto P>
bool operator>=(sek<V, S, P> const &lhs, sek<V, S, P> const &rhs)
{
    return (lhs > rhs) || (lhs == rhs);
}
template<typename V, size_t S, auto P>
bool operator==(sek<V, S, P> const &lhs, sek<V, S, P> const &rhs)
{
    return getsize(lhs) == getsize(rhs) && std::equal(lhs.parallel, lhs.actual, lhs.actual + lhs.size, rhs.actual);
}
template<typename V, size_t S, auto P>
bool operator!=(sek<V, S, P> const &lhs, sek<V, S, P> const &rhs)
{
    return !(lhs == rhs);
}

template<typename V, size_t S, auto P>
std::ostream &operator<<(std::ostream &os, sek<V, S, P> const &container)
{
    auto outputElement = [&](auto element) { os << element << " "; };
    save(outputElement, container);
    return os;
}
template<typename V, size_t S, auto P>
std::istream &operator>>(std::istream &is, sek<V, S, P> &container)
{
    auto inputElement = [&](auto &element) { is >> element; };
    load(inputElement, container);
    return is;
}

template<typename V, size_t S, auto P>
bool isfullysatisfied(sek<V, S, P> const &container, auto predicate)
{
    return std::all_of(container.actual, container.actual + container.size, predicate);
}

template<typename V, size_t S, auto P>
sek<size_t, S, P> indicesfromcondition(sek<V, S, P> const &container, auto predicate)
{
    sek<size_t, S, P> indices;
    auto matchCount = std::count_if(container.parallel, container.actual, container.actual + container.size, predicate);
    setsize(indices, matchCount);
    size_t currentIndex = 0;
    for (size_t pos = 0; pos < container.size; ++pos)
        if (predicate(container[pos]))
            indices[currentIndex++] = pos;
    return indices;
}

template<typename V, size_t S, auto P>
sek<size_t, S, P> indicesfromvalue(sek<V, S, P> const &container, V value, sek<size_t, S, P> const &sortedIndices)
{
    size_t containerSize = container.size;
    size_t left = 0, right = containerSize ? containerSize - 1 : 0;
    sek<size_t, S, P> indices(containerSize);
    while (left <= right && left < containerSize)
    {
        size_t middle = left + (right - left) / 2;
        if (container[sortedIndices[middle]] == value)
        {
            size_t startIndex = middle;
            while (startIndex > 0 && container[sortedIndices[startIndex - 1]] == value)
                --startIndex;
            size_t endIndex = middle;
            while (endIndex < containerSize - 1 && container[sortedIndices[endIndex + 1]] == value)
                ++endIndex;
            size_t matchCount = endIndex - startIndex + 1;
            for (size_t pos = 0; pos < matchCount; ++pos)
                indices[pos] = pos + startIndex;
            setsize(indices, matchCount);
            return indices;
        } else if (container[sortedIndices[middle]] < value)
            left = middle + 1;
        else
        {
            if (middle == 0)
                break;
            right = middle - 1;
        }
    }
    setsize(indices, 0);
    return indices;
}

template<typename V, size_t S, auto P>
sek<size_t, S, P> indicesfromvalue(sek<V, S, P> const &sortedcontainer, V value)
{
    auto rangeResult = std::equal_range(sortedcontainer.actual, sortedcontainer.actual + sortedcontainer.size, value);
    size_t rangeSize = rangeResult.second - rangeResult.first;
    sek<size_t, S, P> indices(rangeSize);
#pragma omp parallel for
    for (size_t pos = 0; pos < rangeSize; ++pos)
        indices[pos] = static_cast<size_t>(rangeResult.first - sortedcontainer.actual) + pos;
    return indices;
}

template<typename V, size_t S, auto P>
sek<size_t, S, P> getorder(sek<V, S, P> const &container)
{
    sek<size_t, S, P> sortedIndices(container.size);
    std::iota(sortedIndices.actual, sortedIndices.actual + sortedIndices.size, 0);
    std::sort(sortedIndices.actual, sortedIndices.actual + sortedIndices.size,
              [&](size_t idx1, size_t idx2) { return container[idx1] < container[idx2]; });
    return sortedIndices;
}

template<typename V, size_t S, auto P>
sek<V, S, P> getasample(sek<V, S, P> const &container, size_t sampleSize)
{
    if (sampleSize > container.size)
        sampleSize = container.size;
    sek<V, S, P> sampleContainer(sampleSize);
    std::random_device randomDevice;
    std::mt19937 rng(randomDevice());
    std::sample(container.actual, container.actual + container.size, sampleContainer.actual, sampleSize, rng);
    return sampleContainer;
}

template<typename V, size_t S, auto P>
void setsorted(sek<V, S, P> &container)
{
    std::stable_sort(container.actual, container.actual + container.size);
}
template<typename V, size_t S, auto P>
void setsorted(sek<V, S, P> &container, auto compareFunc)
{
    std::stable_sort(container.actual, container.actual + container.size, compareFunc);
}

template<typename V, size_t S, auto P>
void applyfunction(sek<V, S, P> const &container, auto func)
{
    std::for_each(container.parallel, container.actual, container.actual + container.size, func);
}

template<typename V, size_t S, auto P>
auto getmap(sek<V, S, P> const &container, auto func)
{
    using ReturnType = decltype(func(std::declval<V>()));
    sek<ReturnType, S, P> mappedContainer(container.size);
    std::transform(container.parallel, container.actual, container.actual + container.size, mappedContainer.actual,
                   func);
    return mappedContainer;
}

template<typename V, size_t S, auto P>
auto getmap(sek<V, S, P> const &containerA, sek<V, S, P> const &containerB, auto func)
{
    size_t minSize = std::min(containerA.size, containerB.size);
    using ReturnType = decltype(func(std::declval<V>(), std::declval<V>()));
    sek<ReturnType, S, P> mappedContainer(minSize);
    std::transform(containerA.parallel, containerA.actual, containerA.actual + minSize, containerB.actual,
                   mappedContainer.actual, func);
    return mappedContainer;
}

template<typename V, size_t S, auto P>
auto reduce(sek<V, S, P> const &container, auto init, auto binaryFunc)
{
    return std::reduce(container.parallel, container.actual, container.actual + container.size, init, binaryFunc);
}

template<typename V, size_t S, auto P>
auto dotproduct(sek<V, S, P> const &containerA, sek<V, S, P> const &containerB, auto init, auto sumFunc, auto prodFunc)
{
    return std::inner_product(containerA.actual, containerA.actual + containerA.size, containerB.actual, init, sumFunc,
                              prodFunc);
}

template<typename V, size_t S, auto P>
void setshuffled(sek<V, S, P> &container)
{
    std::random_device randomDevice;
    std::mt19937 rng(randomDevice());
    std::shuffle(container.actual, container.actual + container.size, rng);
}

template<typename V, size_t S, auto P>
void setreversed(sek<V, S, P> &container)
{
    std::reverse(container.actual, container.actual + container.size);
}

template<typename V, size_t S, auto P>
void setrotatedaroundindex(sek<V, S, P> &container, size_t rotationIndex)
{
    if (rotationIndex > container.size)
        rotationIndex = container.size;
    std::rotate(container.actual, container.actual + rotationIndex, container.actual + container.size);
}

template<typename V, size_t S, auto P>
bool setpermuteclockwise(sek<V, S, P> &container)
{
    return std::next_permutation(container.actual, container.actual + container.size);
}

template<typename V, size_t S, auto P>
bool setpermutecounterclockwise(sek<V, S, P> &container)
{
    return std::prev_permutation(container.actual, container.actual + container.size);
}

template<typename V, size_t S, auto P>
void setunique(sek<V, S, P> &sortedcontainer)
{
    auto newEnd = std::unique(sortedcontainer.actual, sortedcontainer.actual + sortedcontainer.size);
    setsize(sortedcontainer, newEnd - sortedcontainer.actual);
}

template<typename V, size_t S, auto P>
void indicesfromorder(sek<V, S, P> const &sourceContainer, sek<size_t, S, P> const &sortedIndices,
                      sek<size_t, S, P> &mappingIndices, sek<size_t, S, P> &firstSortedIndices)
{
    size_t totalSize = getsize(sourceContainer);
    setsize(firstSortedIndices, totalSize);
    if (!totalSize)
    {
        setsize(mappingIndices, 0);
        return;
    }
    size_t firstOccurrence = sortedIndices[0];
    firstSortedIndices[firstOccurrence] = firstOccurrence;
    for (size_t index = 1; index < totalSize; ++index)
    {
        size_t currentSortedIndex = sortedIndices[index];
        firstSortedIndices[currentSortedIndex] =
                (sourceContainer[sortedIndices[index]] == sourceContainer[sortedIndices[index - 1]])
                        ? firstOccurrence
                        : currentSortedIndex;
        if (sourceContainer[sortedIndices[index]] != sourceContainer[sortedIndices[index - 1]])
            firstOccurrence = currentSortedIndex;
    }
    size_t uniqueCount = 0;
    for (size_t index = 0; index < totalSize; ++index)
        if (firstSortedIndices[index] == index)
            firstSortedIndices[uniqueCount++] = index;
    setsize(firstSortedIndices, uniqueCount);
    setsize(mappingIndices, totalSize);
#pragma omp parallel for
    for (size_t index = 0; index < totalSize; ++index)
        mappingIndices[index] = static_cast<size_t>(-1);
#pragma omp parallel for
    for (size_t uniqueIndex = 0; uniqueIndex < uniqueCount; ++uniqueIndex)
        mappingIndices[firstSortedIndices[uniqueIndex]] = uniqueIndex;
#pragma omp parallel for
    for (size_t index = 0; index < totalSize; ++index)
        if (mappingIndices[index] == static_cast<size_t>(-1))
            mappingIndices[index] = mappingIndices[firstSortedIndices[index]];
}

template<typename V, size_t S, auto P>
sek<V, S, P> getintersection(sek<V, S, P> const &sortedcontainerA, sek<V, S, P> const &sortedcontainerB)
{
    sek<V, S, P> resultContainer(sortedcontainerA.size + sortedcontainerB.size);
    auto endIt = std::set_intersection(sortedcontainerA.actual, sortedcontainerA.actual + sortedcontainerA.size,
                                       sortedcontainerB.actual, sortedcontainerB.actual + sortedcontainerB.size,
                                       resultContainer.actual);
    setsize(resultContainer, static_cast<size_t>(endIt - resultContainer.actual));
    return resultContainer;
}

template<typename V, size_t S, auto P>
sek<V, S, P> getunion(sek<V, S, P> const &sortedcontainerA, sek<V, S, P> const &sortedcontainerB)
{
    sek<V, S, P> resultContainer(sortedcontainerA.size + sortedcontainerB.size);
    auto endIt = std::set_union(sortedcontainerA.actual, sortedcontainerA.actual + sortedcontainerA.size,
                                sortedcontainerB.actual, sortedcontainerB.actual + sortedcontainerB.size,
                                resultContainer.actual);
    setsize(resultContainer, static_cast<size_t>(endIt - resultContainer.actual));
    return resultContainer;
}

template<typename V, size_t S, auto P>
sek<V, S, P> getdifference(sek<V, S, P> const &sortedcontainerA, sek<V, S, P> const &sortedcontainerB)
{
    sek<V, S, P> resultContainer(sortedcontainerA.size + sortedcontainerB.size);
    auto endIt = std::set_difference(sortedcontainerA.actual, sortedcontainerA.actual + sortedcontainerA.size,
                                     sortedcontainerB.actual, sortedcontainerB.actual + sortedcontainerB.size,
                                     resultContainer.actual);
    setsize(resultContainer, static_cast<size_t>(endIt - resultContainer.actual));
    return resultContainer;
}

template<typename V, size_t S, auto P>
sek<V, S, P> getsymmetricdifference(sek<V, S, P> const &sortedcontainerA, sek<V, S, P> const &sortedcontainerB)
{
    sek<V, S, P> resultContainer(sortedcontainerA.size + sortedcontainerB.size);
    auto endIt = std::set_symmetric_difference(sortedcontainerA.actual, sortedcontainerA.actual + sortedcontainerA.size,
                                               sortedcontainerB.actual, sortedcontainerB.actual + sortedcontainerB.size,
                                               resultContainer.actual);
    setsize(resultContainer, static_cast<size_t>(endIt - resultContainer.actual));
    return resultContainer;
}

template<typename V, size_t S, auto P>
bool getincludessubset(sek<V, S, P> const &sortedsuperset, sek<V, S, P> const &sortedsubset)
{
    return std::includes(sortedsuperset.actual, sortedsuperset.actual + sortedsuperset.size, sortedsubset.actual,
                         sortedsubset.actual + sortedsubset.size);
}

template<typename V, size_t S, auto P>
size_t getindextoinsert(sek<V, S, P> const &sortedcontainer, V value)
{
    return static_cast<size_t>(
            std::lower_bound(sortedcontainer.actual, sortedcontainer.actual + sortedcontainer.size, value) -
            sortedcontainer.actual);
}

template<typename V, size_t S, auto P>
size_t addretainingorder(sek<V, S, P> &sortedcontainer, V value)
{
    size_t insertPosition = getindextoinsert(sortedcontainer, value);
    addinplace(sortedcontainer, insertPosition, value);
    return insertPosition;
}

#endif
