#ifndef SEK_HPP
#define SEK_HPP

#include <algorithm>
#include <cassert>
#include <cmath>
#include <execution>
#include <initializer_list>
#include <numeric>
#include <random>
#include <stdexcept>
#include <type_traits>
#ifdef _OPENMP
#include <omp.h>
#endif

template <typename T> inline void arraydestroy(T *&p) {
  if (p) {
    delete[] p;
    p = nullptr;
  }
}

template <typename T> inline void arraycreate(T *&p, size_t size) {
  arraydestroy(p);
  if (size > 0) {
    p = new T[size];
    std::fill(p, p + size, T());
  }
}

namespace hidden {
constexpr size_t STACKSIZE = 5;
}

template <typename V, size_t S = hidden::STACKSIZE,
          auto P = std::execution::par>
struct seque {
  static constexpr size_t stacksize = S;
  static constexpr auto parallel = P;
  size_t size = 0;
  V stackdata[S];
  V *heapdata = nullptr;
  size_t heapsize = 0;
  V *actual = stackdata;
  seque() {
    for (size_t i = 0; i < stacksize; i++) {
      stackdata[i] = V();
    }
  }
  explicit seque(size_t newSize) : seque() { setsize(*this, newSize); }
  seque(size_t newSize, V value) : seque(newSize) {
    for (size_t i = 0; i < size; i++) {
      actual[i] = value;
    }
  }
  seque(std::initializer_list<V> const &values) {
    setsize(*this, values.size());
    std::copy(values.begin(), values.end(), actual);
  }
  seque(seque const &other) : seque() { copy_from(other); }
  seque(seque &&other) noexcept : seque() { move_from(std::move(other)); }
  ~seque() { setsize(*this, 0); }
  seque &operator=(seque const &other) {
    if (this != &other) {
      copy_from(other);
    }
    return *this;
  }
  seque &operator=(seque &&other) noexcept {
    if (this != &other) {
      move_from(std::move(other));
    }
    return *this;
  }
  seque &operator=(V value) {
// Each element is set independently.
#pragma omp parallel for if (size > 1000)
    for (size_t i = 0; i < size; i++) {
      actual[i] = value;
    }
    return *this;
  }
  V &operator[](size_t index) { return actual[index]; }
  V const &operator[](size_t index) const { return actual[index]; }
  V &operator()(size_t index) {
    if (index >= size) {
      setsize(*this, index + 1);
    }
    return actual[index];
  }
  V const &operator()(size_t index) const {
    _explodeinvalidindex(index);
    return actual[index];
  }
  seque &operator=(std::initializer_list<V> const &initList) {
    setsize(*this, initList.size());
    std::copy(initList.begin(), initList.end(), actual);
    return *this;
  }
  V *begin() { return actual; }
  V *end() { return actual + size; }
  V const *begin() const { return actual; }
  V const *end() const { return actual + size; }
  seque operator()(seque<size_t, S, P> const &indexContainer) const {
    seque result(indexContainer.size);
    std::transform(indexContainer.actual,
                   indexContainer.actual + indexContainer.size, result.actual,
                   [&](size_t idx) { return actual[idx]; });
    return result;
  }

private:
  void copy_from(seque const &other) {
    setsize(*this, other.size);
    std::copy(parallel, other.actual, other.actual + other.size, actual);
  }
  void move_from(seque &&other) noexcept {
    if (this != &other) {
      if (heapdata && actual == heapdata) {
        arraydestroy(heapdata);
        heapdata = nullptr;
        heapsize = 0;
      }
      for (size_t i = 0; i < stacksize; i++) {
        stackdata[i] = V();
      }
      size = other.size;
      if (other.actual == other.stackdata) {
        actual = stackdata;
        std::move(other.stackdata, other.stackdata + other.size, stackdata);
      } else {
        heapdata = other.heapdata;
        heapsize = other.heapsize;
        actual = heapdata;
        other.heapdata = nullptr;
        other.heapsize = 0;
      }
      other.size = 0;
      other.actual = other.stackdata;
    }
  }

public:
  void _explodeinvalidindex(size_t index) const {
    if (index >= size) {
      throw std::out_of_range("sek: index " + std::to_string(index) +
                              " out of range [0, " + std::to_string(size) +
                              ").");
    }
  }
  void _switchfromstacktoheap(size_t newCapacity) {
    heapsize = newCapacity;
    arraycreate(heapdata, newCapacity);
#pragma omp parallel for if (size > 1000)
    for (size_t i = 0; i < size; i++) {
      heapdata[i] = stackdata[i];
    }
    actual = heapdata;
  }
  void _switchfromheapstostack() {
    const size_t moveCount = std::min(heapsize, stacksize);
    if (heapdata) {
      if (moveCount > 0) {
#pragma omp parallel for if (moveCount > 1000)
        for (size_t i = 0; i < moveCount; i++) {
          stackdata[i] = heapdata[i];
        }
      }
      arraydestroy(heapdata);
      heapdata = nullptr;
    }
    heapsize = 0;
    actual = stackdata;
  }
  void _modifysizeofheap(size_t newCapacity) {
    if (newCapacity > heapsize) {
      V *tempArray = nullptr;
      arraycreate(tempArray, newCapacity);
#pragma omp parallel for if (size > 1000)
      for (size_t i = 0; i < size; i++) {
        tempArray[i] = std::move(heapdata[i]);
      }
      arraydestroy(heapdata);
      heapdata = tempArray;
      heapsize = newCapacity;
      actual = heapdata;
    }
  }
};

template <typename V, size_t S, auto P>
inline V *begin(seque<V, S, P> &container) {
  return container.begin();
}

template <typename V, size_t S, auto P>
inline V *end(seque<V, S, P> &container) {
  return container.end();
}

template <typename V, size_t S, auto P>
void setsize(seque<V, S, P> &container, size_t newSize,
             size_t growthFactor = 2) {
  assert(growthFactor >= 2);
  if (container.actual == container.stackdata) {
    if (newSize > container.stacksize) {
      container._switchfromstacktoheap(growthFactor * newSize);
    }
  } else {
    if (newSize <= container.stacksize) {
      container._switchfromheapstostack();
    } else if (newSize > container.heapsize) {
      container._modifysizeofheap(growthFactor * newSize);
    }
  }
  container.size = newSize;
}

template <typename V, size_t S, auto P>
void save(auto archiver, seque<V, S, P> const &container) {
  archiver(container.size);
  for (size_t index = 0; index < container.size; ++index) {
    archiver(container.actual[index]);
  }
}

template <typename V, size_t S, auto P>
void load(auto archiver, seque<V, S, P> &container) {
  size_t loadedSize = 0;
  archiver(loadedSize);
  setsize(container, loadedSize);
  for (size_t index = 0; index < container.size; ++index) {
    archiver(container.actual[index]);
  }
}

template <typename V, size_t S, auto P>
void swap(seque<V, S, P> &containerA, seque<V, S, P> &containerB) {
  if (containerA.actual == containerA.heapdata &&
      containerB.actual == containerB.heapdata) {
    std::swap(containerA.heapdata, containerB.heapdata);
    std::swap(containerA.actual, containerB.actual);
    std::swap(containerA.heapsize, containerB.heapsize);
    std::swap(containerA.size, containerB.size);
  } else {
    auto temp = containerA;
    containerA = containerB;
    containerB = temp;
  }
}

template <typename V, size_t S, auto P>
bool isindexvalid(seque<V, S, P> const &container, size_t index) {
  return (index < container.size);
}

template <typename V, size_t S, auto P> void erase(seque<V, S, P> &container) {
  setsize(container, 0);
}

template <typename V, size_t S, auto P>
void erase(seque<V, S, P> &container, size_t eraseIndex) {
  if (isindexvalid(container, eraseIndex) && container.size > 0) {
    container.actual[eraseIndex] = container.actual[container.size - 1];
    container.actual[container.size - 1] = V();
    setsize(container, container.size - 1);
  }
}

template <typename V, size_t S, auto P>
void erase(seque<V, S, P> &container,
           seque<size_t, S, P> const &indexContainer) {
  for (size_t pos = 0; pos < indexContainer.size; ++pos) {
    erase(container, indexContainer[pos]);
  }
}

template <typename V, size_t S, auto P>
void eraselast(seque<V, S, P> &container) {
  if (container.size > 0) {
    setsize(container, container.size - 1);
  }
}

template <typename V, size_t S, auto P>
void eraseinplace(seque<V, S, P> &container, size_t startIndex,
                  size_t numToErase = 1) {
  if (!numToErase || startIndex >= container.size)
    return;
  if (startIndex + numToErase > container.size) {
    numToErase = container.size - startIndex;
  }
  std::move(container.actual + startIndex + numToErase,
            container.actual + container.size, container.actual + startIndex);
  setsize(container, container.size - numToErase);
}

template <typename V, size_t S, auto P>
void add(seque<V, S, P> &container, size_t insertIndex, V value) {
  container._explodeinvalidindex(insertIndex);
  setsize(container, container.size + 1);
  container[container.size - 1] = container[insertIndex];
  container[insertIndex] = value;
}

template <typename V, size_t S, auto P>
void addinplace(seque<V, S, P> &container, size_t insertIndex, V value) {
  if (insertIndex > container.size) {
    insertIndex = container.size;
  }
  setsize(container, container.size + 1);
  std::move_backward(container.actual + insertIndex,
                     container.actual + (container.size - 1),
                     container.actual + container.size);
  container[insertIndex] = value;
}

template <typename V, size_t S, auto P>
void addinplace(seque<V, S, P> &destContainer, size_t insertIndex,
                seque<V, S, P> const &sourceContainer) {
  if (insertIndex > destContainer.size) {
    insertIndex = destContainer.size;
  }
  destContainer._explodeinvalidindex(insertIndex == 0 ? 0 : (insertIndex - 1));
  const size_t oldSize = destContainer.size;
  setsize(destContainer, destContainer.size + sourceContainer.size);
  std::move_backward(destContainer.actual + insertIndex,
                     destContainer.actual + oldSize,
                     destContainer.actual + destContainer.size);
  std::copy(destContainer.parallel, sourceContainer.actual,
            sourceContainer.actual + sourceContainer.size,
            destContainer.actual + insertIndex);
}

template <typename V, size_t S, auto P>
size_t append(seque<V, S, P> &container, V value) {
  setsize(container, container.size + 1);
  container[container.size - 1] = value;
  return container.size - 1;
}

template <typename V, size_t S, auto P>
size_t append(seque<V, S, P> &destContainer,
              seque<V, S, P> const &sourceContainer) {
  const size_t oldSize = destContainer.size;
  setsize(destContainer, destContainer.size + sourceContainer.size);
  std::copy(destContainer.parallel, sourceContainer.actual,
            sourceContainer.actual + sourceContainer.size,
            destContainer.actual + oldSize);
  return oldSize - 1;
}

template <typename V, size_t S, auto P>
size_t append(seque<V, S, P> &destContainer, seque<V, S, P> &&sourceContainer) {
  const size_t oldSize = destContainer.size;
  setsize(destContainer, destContainer.size + sourceContainer.size);
  std::move(sourceContainer.actual,
            sourceContainer.actual + sourceContainer.size,
            destContainer.actual + oldSize);
  return oldSize - 1;
}

template <typename V, size_t S, auto P>
size_t getsize(seque<V, S, P> const &container) {
  return container.size;
}

template <typename V, size_t S, auto P>
bool operator<(seque<V, S, P> const &lhs, seque<V, S, P> const &rhs) {
  return std::lexicographical_compare(
      lhs.actual, lhs.actual + lhs.size, rhs.actual, rhs.actual + rhs.size,
      [](auto const &a, auto const &b) { return a < b; });
}

template <typename V, size_t S, auto P>
bool operator>(seque<V, S, P> const &lhs, seque<V, S, P> const &rhs) {
  return rhs < lhs;
}

template <typename V, size_t S, auto P>
bool operator>=(seque<V, S, P> const &lhs, seque<V, S, P> const &rhs) {
  return (lhs > rhs) || (lhs == rhs);
}

template <typename V, size_t S, auto P>
bool operator==(seque<V, S, P> const &lhs, seque<V, S, P> const &rhs) {
  if (lhs.size != rhs.size)
    return false;
  return std::equal(lhs.parallel, lhs.actual, lhs.actual + lhs.size,
                    rhs.actual);
}

template <typename V, size_t S, auto P>
bool operator!=(seque<V, S, P> const &lhs, seque<V, S, P> const &rhs) {
  return !(lhs == rhs);
}

template <typename V, size_t S, auto P>
std::ostream &operator<<(std::ostream &os, seque<V, S, P> const &container) {
  auto outputElement = [&](auto element) { os << element << " "; };
  save(outputElement, container);
  return os;
}

template <typename V, size_t S, auto P>
std::istream &operator>>(std::istream &is, seque<V, S, P> &container) {
  auto inputElement = [&](auto &element) { is >> element; };
  load(inputElement, container);
  return is;
}

template <typename V, size_t S, auto P>
bool isfullysatisfied(seque<V, S, P> const &container, auto predicate) {
  return std::all_of(container.actual, container.actual + container.size,
                     predicate);
}

template <typename V, size_t S, auto P>
seque<size_t, S, P> indicesfromcondition(seque<V, S, P> const &container,
                                         auto predicate) {
  seque<size_t, S, P> indices;
  auto matchCount = std::count_if(container.parallel, container.actual,
                                  container.actual + container.size, predicate);
  setsize(indices, matchCount);
  size_t currentIndex = 0;
  for (size_t pos = 0; pos < container.size; ++pos) {
    if (predicate(container[pos])) {
      indices[currentIndex++] = pos;
    }
  }
  return indices;
}

template <typename V, size_t S, auto P>
seque<size_t, S, P> indicesfromvalue(seque<V, S, P> const &container, V value,
                                     seque<size_t, S, P> const &sortedIndices) {
  size_t containerSize = container.size;
  if (containerSize == 0) {
    return seque<size_t, S, P>{};
  }
  size_t left = 0;
  size_t right = containerSize - 1;
  seque<size_t, S, P> indices(containerSize);
  while (left <= right && left < containerSize) {
    size_t middle = left + (right - left) / 2;
    auto valMid = container[sortedIndices[middle]];
    if (valMid == value) {
      size_t startIndex = middle;
      while (startIndex > 0 &&
             container[sortedIndices[startIndex - 1]] == value)
        --startIndex;
      size_t endIndex = middle;
      while (endIndex < containerSize - 1 &&
             container[sortedIndices[endIndex + 1]] == value)
        ++endIndex;
      size_t matchCount = endIndex - startIndex + 1;
      for (size_t pos = 0; pos < matchCount; ++pos) {
        indices[pos] = sortedIndices[startIndex + pos];
      }
      setsize(indices, matchCount);
      return indices;
    } else if (valMid < value) {
      left = middle + 1;
    } else {
      if (middle == 0)
        break;
      right = middle - 1;
    }
  }
  setsize(indices, 0);
  return indices;
}

template <typename V, size_t S, auto P>
seque<size_t, S, P> indicesfromvalue(seque<V, S, P> const &sortedcontainer,
                                     V value) {
  auto rangeResult =
      std::equal_range(sortedcontainer.actual,
                       sortedcontainer.actual + sortedcontainer.size, value);
  size_t rangeSize = rangeResult.second - rangeResult.first;
  seque<size_t, S, P> indices(rangeSize);
  for (size_t pos = 0; pos < rangeSize; ++pos) {
    indices[pos] =
        static_cast<size_t>((rangeResult.first - sortedcontainer.actual) + pos);
  }
  return indices;
}

template <typename V, size_t S, auto P>
seque<size_t, S, P> getorder(seque<V, S, P> const &container) {
  seque<size_t, S, P> sortedIndices(container.size);
  std::iota(sortedIndices.actual, sortedIndices.actual + sortedIndices.size, 0);
  std::sort(sortedIndices.actual, sortedIndices.actual + sortedIndices.size,
            [&](size_t idx1, size_t idx2) {
              return container[idx1] < container[idx2];
            });
  return sortedIndices;
}

template <typename V, size_t S, auto P>
seque<V, S, P> getasample(seque<V, S, P> const &container, size_t sampleSize) {
  if (sampleSize > container.size) {
    sampleSize = container.size;
  }
  seque<V, S, P> sampleContainer(sampleSize);
  std::random_device rd;
  std::mt19937 rng(rd());
  std::sample(container.actual, container.actual + container.size,
              sampleContainer.actual, sampleSize, rng);
  return sampleContainer;
}

template <typename V, size_t S, auto P>
void setordered(seque<V, S, P> &container) {
  std::stable_sort(container.actual, container.actual + container.size);
}

template <typename V, size_t S, auto P>
void setordered(seque<V, S, P> &container, auto compareFunc) {
  std::stable_sort(container.actual, container.actual + container.size,
                   compareFunc);
}

template <typename V, size_t S, auto P>
void applyfunction(seque<V, S, P> const &container, auto func) {
  std::for_each(container.parallel, container.actual,
                container.actual + container.size, func);
}

template <typename V, size_t S, auto P>
auto getmapped(seque<V, S, P> const &container, auto func) {
  using ReturnType = decltype(func(std::declval<V>()));
  seque<ReturnType, S, P> mappedContainer(container.size);
  std::transform(container.parallel, container.actual,
                 container.actual + container.size, mappedContainer.actual,
                 func);
  return mappedContainer;
}

template <typename V, size_t S, auto P>
auto getmapped(seque<V, S, P> const &containerA,
               seque<V, S, P> const &containerB, auto func) {
  const size_t minSize = (std::min)(containerA.size, containerB.size);
  using ReturnType = decltype(func(std::declval<V>(), std::declval<V>()));
  seque<ReturnType, S, P> mappedContainer(minSize);
  std::transform(containerA.parallel, containerA.actual,
                 containerA.actual + minSize, containerB.actual,
                 mappedContainer.actual, func);
  return mappedContainer;
}

template <typename V, size_t S, auto P>
auto reduce(seque<V, S, P> const &container, auto init, auto binaryFunc) {
  return std::reduce(container.parallel, container.actual,
                     container.actual + container.size, init, binaryFunc);
}

template <typename V, size_t S, auto P>
auto dotproduct(seque<V, S, P> const &containerA,
                seque<V, S, P> const &containerB, auto init, auto sumFunc,
                auto prodFunc) {
  return std::inner_product(containerA.actual,
                            containerA.actual + containerA.size,
                            containerB.actual, init, sumFunc, prodFunc);
}

template <typename V, size_t S, auto P>
void setshuffled(seque<V, S, P> &container) {
  std::random_device rd;
  std::mt19937 rng(rd());
  std::shuffle(container.actual, container.actual + container.size, rng);
}

template <typename V, size_t S, auto P>
void setreversed(seque<V, S, P> &container) {
  std::reverse(container.actual, container.actual + container.size);
}

template <typename V, size_t S, auto P>
void setrotatedaroundindex(seque<V, S, P> &container, size_t rotationIndex) {
  if (rotationIndex > container.size) {
    rotationIndex = container.size;
  }
  std::rotate(container.actual, container.actual + rotationIndex,
              container.actual + container.size);
}

template <typename V, size_t S, auto P>
bool setpermuteclockwise(seque<V, S, P> &container) {
  return std::next_permutation(container.actual,
                               container.actual + container.size);
}

template <typename V, size_t S, auto P>
bool setpermutecounterclockwise(seque<V, S, P> &container) {
  return std::prev_permutation(container.actual,
                               container.actual + container.size);
}

template <typename V, size_t S, auto P>
void setunique(seque<V, S, P> &sortedcontainer) {
  auto newEnd = std::unique(sortedcontainer.actual,
                            sortedcontainer.actual + sortedcontainer.size);
  setsize(sortedcontainer,
          static_cast<size_t>(newEnd - sortedcontainer.actual));
}

template <typename V, size_t S, auto P>
void setorderedandunique(seque<V, S, P> &container) {
  setordered(container);
  setunique(container);
}

template <typename V, size_t S, auto P>
void indicesfromorder(seque<V, S, P> const &sourceContainer,
                      seque<size_t, S, P> const &sortedIndices,
                      seque<size_t, S, P> &mappingIndices,
                      seque<size_t, S, P> &firstSortedIndices) {
  size_t totalSize = getsize(sourceContainer);
  setsize(firstSortedIndices, totalSize);
  if (!totalSize) {
    setsize(mappingIndices, 0);
    return;
  }
  size_t firstOccurrence = sortedIndices[0];
  firstSortedIndices[firstOccurrence] = firstOccurrence;
  for (size_t index = 1; index < totalSize; ++index) {
    size_t currentSortedIndex = sortedIndices[index];
    bool sameVal = (sourceContainer[sortedIndices[index]] ==
                    sourceContainer[sortedIndices[index - 1]]);
    firstSortedIndices[currentSortedIndex] =
        sameVal ? firstOccurrence : currentSortedIndex;
    if (!sameVal) {
      firstOccurrence = currentSortedIndex;
    }
  }
  size_t uniqueCount = 0;
  for (size_t index = 0; index < totalSize; ++index) {
    if (firstSortedIndices[index] == index) {
      firstSortedIndices[uniqueCount++] = index;
    }
  }
  setsize(firstSortedIndices, uniqueCount);
  setsize(mappingIndices, totalSize);
// Parallelize initialization of mappingIndices
#pragma omp parallel for if (totalSize > 1000)
  for (size_t index = 0; index < totalSize; ++index) {
    mappingIndices[index] = static_cast<size_t>(-1);
  }
  for (size_t uniqueIndex = 0; uniqueIndex < uniqueCount; ++uniqueIndex) {
    mappingIndices[firstSortedIndices[uniqueIndex]] = uniqueIndex;
  }
// Parallelize the final update loop
#pragma omp parallel for if (totalSize > 1000)
  for (size_t index = 0; index < totalSize; ++index) {
    if (mappingIndices[index] == static_cast<size_t>(-1)) {
      mappingIndices[index] = mappingIndices[firstSortedIndices[index]];
    }
  }
}

template <typename V, size_t S, auto P>
seque<V, S, P> getintersection(seque<V, S, P> const &sortedcontainerA,
                               seque<V, S, P> const &sortedcontainerB) {
  seque<V, S, P> resultContainer(sortedcontainerA.size + sortedcontainerB.size);
  auto endIt = std::set_intersection(
      sortedcontainerA.actual, sortedcontainerA.actual + sortedcontainerA.size,
      sortedcontainerB.actual, sortedcontainerB.actual + sortedcontainerB.size,
      resultContainer.actual);
  setsize(resultContainer, static_cast<size_t>(endIt - resultContainer.actual));
  return resultContainer;
}

template <typename V, size_t S, auto P>
seque<V, S, P> getunion(seque<V, S, P> const &sortedcontainerA,
                        seque<V, S, P> const &sortedcontainerB) {
  seque<V, S, P> resultContainer(sortedcontainerA.size + sortedcontainerB.size);
  auto endIt = std::set_union(
      sortedcontainerA.actual, sortedcontainerA.actual + sortedcontainerA.size,
      sortedcontainerB.actual, sortedcontainerB.actual + sortedcontainerB.size,
      resultContainer.actual);
  setsize(resultContainer, static_cast<size_t>(endIt - resultContainer.actual));
  return resultContainer;
}

template <typename V, size_t S, auto P>
seque<V, S, P> getdifference(seque<V, S, P> const &sortedcontainerA,
                             seque<V, S, P> const &sortedcontainerB) {
  seque<V, S, P> resultContainer(sortedcontainerA.size + sortedcontainerB.size);
  auto endIt = std::set_difference(
      sortedcontainerA.actual, sortedcontainerA.actual + sortedcontainerA.size,
      sortedcontainerB.actual, sortedcontainerB.actual + sortedcontainerB.size,
      resultContainer.actual);
  setsize(resultContainer, static_cast<size_t>(endIt - resultContainer.actual));
  return resultContainer;
}

template <typename V, size_t S, auto P>
seque<V, S, P> getsymmetricdifference(seque<V, S, P> const &sortedcontainerA,
                                      seque<V, S, P> const &sortedcontainerB) {
  seque<V, S, P> resultContainer(sortedcontainerA.size + sortedcontainerB.size);
  auto endIt = std::set_symmetric_difference(
      sortedcontainerA.actual, sortedcontainerA.actual + sortedcontainerA.size,
      sortedcontainerB.actual, sortedcontainerB.actual + sortedcontainerB.size,
      resultContainer.actual);
  setsize(resultContainer, static_cast<size_t>(endIt - resultContainer.actual));
  return resultContainer;
}

template <typename V, size_t S, auto P>
bool getincludessubset(seque<V, S, P> const &sortedsuperset,
                       seque<V, S, P> const &sortedsubset) {
  return std::includes(
      sortedsuperset.actual, sortedsuperset.actual + sortedsuperset.size,
      sortedsubset.actual, sortedsubset.actual + sortedsubset.size);
}

template <typename V, size_t S, auto P>
size_t getindextoinsert(seque<V, S, P> const &sortedcontainer, V value) {
  return static_cast<size_t>(
      std::lower_bound(sortedcontainer.actual,
                       sortedcontainer.actual + sortedcontainer.size, value) -
      sortedcontainer.actual);
}

template <typename V, size_t S, auto P>
size_t addretainingorder(seque<V, S, P> &sortedcontainer, V value) {
  size_t insertPosition = getindextoinsert(sortedcontainer, value);
  addinplace(sortedcontainer, insertPosition, value);
  return insertPosition;
}

#endif
