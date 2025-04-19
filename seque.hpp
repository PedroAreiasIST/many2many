#ifndef SEK_HPP
#define SEK_HPP

#include "basics.hpp"
#include <algorithm>
#include <cassert>
#include <cstring> // for std::memcpy
#include <execution>
#include <functional> // For std::hash
#include <initializer_list>
#include <numeric>
#include <random>
#include <stdexcept>
#include <type_traits>

template <typename T> inline void arraydestroy(T *&p) {
  if (p) {
    delete[] p;
    p = nullptr;
  }
}

template <typename T> inline void arraycreate(T *&p, int size) {
  arraydestroy(p);
  if (size > 0) {
    p = new T[size];
  }
}

namespace hidden {
constexpr int STACKSIZE = 4;
constexpr double GROWTHFACTOR = 1.2;
} // namespace hidden

template <typename V, int S = hidden::STACKSIZE, auto P = std::execution::par>
struct seque {
  static constexpr int stacksize = S;
  static constexpr auto parallel = P;
  int size = 0;
  V stackdata[S];
  V *heapdata = nullptr;
  int heapsize = 0;
  V *actual = stackdata;

  seque() {}

  explicit seque(int newSize) : seque() { setsize(*this, newSize); }

  seque(int newSize, V value) : seque(newSize) {
    for (int i = 0; i < size; i++) {
      actual[i] = value;
    }
  }

  seque(std::initializer_list<V> const &values) {
    setsize(*this, values.size());
    std::copy(values.begin(), values.end(), actual);
  }

  seque(seque const &other) : seque() { copy_from(other); }

  seque(seque &&other) noexcept {
    size = other.size;
    // Check if the other container is using its stack storage.
    if (other.actual == other.stackdata) {
      // Move elements one-by-one into our stackdata.
      for (int i = 0; i < other.size; i++) {
        stackdata[i] = std::move(other.stackdata[i]);
      }
      actual = stackdata;
      heapdata = nullptr;
      heapsize = 0;
    } else {
      // Other is using heap storage; steal its heap pointer.
      heapdata = other.heapdata;
      heapsize = other.heapsize;
      actual = heapdata;
    }
    // Reset the other container to a safe empty state.
    other.size = 0;
    other.heapdata = nullptr;
    other.heapsize = 0;
    other.actual = other.stackdata;
  }

  seque &operator=(seque &&other) noexcept {
    if (this != &other) {
      // Free our current dynamic memory if allocated.
      if (heapdata) {
        delete[] heapdata;
        heapdata = nullptr;
      }
      // Copy data from the source.
      size = other.size;
      if (other.actual == other.stackdata) {
        // Source is using stack storage; move element by element.
        for (int i = 0; i < other.size; i++) {
          stackdata[i] = std::move(other.stackdata[i]);
        }
        actual = stackdata;
        heapdata = nullptr;
        heapsize = 0;
      } else {
        // Source is using heap storage; take ownership.
        heapdata = other.heapdata;
        heapsize = other.heapsize;
        actual = heapdata;
      }
      // Reset the source container.
      other.size = 0;
      other.heapdata = nullptr;
      other.heapsize = 0;
      other.actual = other.stackdata;
    }
    return *this;
  }

  void swap(seque &other) noexcept {
    using std::swap;
    // Swap the nelems and heapsize
    swap(size, other.size);
    swap(heapsize, other.heapsize);
    // Swap the heapdata pointers.
    swap(heapdata, other.heapdata);
    // The internal fixed-nelems buffers cannot be swapped overall,
    // but we swap the elements in the buffer up to the minimum nelems.
    int minStack = (S < other.stacksize ? S : other.stacksize);
    for (int i = 0; i < minStack; ++i) {
      swap(stackdata[i], other.stackdata[i]);
    }
    // Reset the actual pointers based on the new state.
    actual = (heapdata ? heapdata : stackdata);
    other.actual = (other.heapdata ? other.heapdata : other.stackdata);
  }

  ~seque() { setsize(*this, 0); }

  seque &operator=(seque const &other) {
    if (this != &other) {
      copy_from(other);
    }
    return *this;
  }

  seque &operator=(V const &value) {
    std::fill(std::execution::par, actual, actual + size, value);
    return *this;
  }

  V &operator[](int index) { return actual[index]; }

  V const &operator[](int index) const { return actual[index]; }

  V &operator()(int index) {
    if (index >= size) {
      setsize(*this, index + 1);
    }
    return actual[index];
  }

  V const &operator()(int index) const {
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

  seque operator()(seque<int, S, P> const &indexContainer) const {
    seque result;
    if (size > 0) {
      setsize(result, indexContainer.size);
      std::transform(std::execution::par, indexContainer.actual,
                     indexContainer.actual + indexContainer.size, result.actual,
                     [&](int idx) {
                       if (idx > size)
                         throw std::out_of_range("sek: index " +
                                                 std::to_string(idx) +
                                                 " out of range [0, " +
                                                 std::to_string(size) + ").)");
                       return actual[idx];
                     });
    }
    return result;
  }

  // Bulk copy optimization:
  void copy_from(seque const &other) {
    setsize(*this, other.size);
    if constexpr (std::is_trivially_copyable_v<V>) {
      std::memcpy(actual, other.actual, other.size * sizeof(V));
    } else {
      std::copy(std::execution::par, other.actual, other.actual + other.size,
                actual);
    }
  }

public:
  void _explodeinvalidindex(int index) const {
    if (index < 0 || index >= size) {
      throw std::out_of_range("sek: index " + std::to_string(index) +
                              " out of range [0, " + std::to_string(size) +
                              ").");
    }
  }

  void _switchfromstacktoheap(int newCapacity) {
    heapsize = newCapacity;
    arraycreate(heapdata, newCapacity);
    if constexpr (std::is_trivially_copyable_v<V>) {
      std::memcpy(heapdata, stackdata,
                  std::min(heapsize, stacksize) * sizeof(V));
    } else {
      for (int i = 0; i < std::min(heapsize, stacksize); i++) {
        heapdata[i] = stackdata[i];
      }
    }
    actual = heapdata;
  }

  void _switchfromheapstostack() {
    const int moveCount = std::min({size, heapsize, stacksize});
    if (heapdata) {
      if (moveCount > 0)
        std::copy(heapdata, heapdata + moveCount, stackdata);
      arraydestroy(heapdata);
      heapdata = nullptr;
    }
    heapsize = 0;
    actual = stackdata;
  }

  void _modifysizeofheap(int newCapacity) {
    if (newCapacity > heapsize) {
      V *tempArray = nullptr;
      arraycreate(tempArray, newCapacity);
      if constexpr (std::is_trivially_copyable_v<V>) {
        std::memcpy(tempArray, heapdata, size * sizeof(V));
      } else {
        std::move(std::execution::par, heapdata, heapdata + size, tempArray);
      }
      arraydestroy(heapdata);
      heapdata = tempArray;
      heapsize = newCapacity;
      actual = heapdata;
    }
  }
};

template <typename V, int S, auto P>
inline V *begin(seque<V, S, P> &container) {
  return container.begin();
}

template <typename V, int S, auto P> inline V *end(seque<V, S, P> &container) {
  return container.end();
}

template <typename V, int S, auto P>
void setsize(seque<V, S, P> &container, int newSize) {
  double growthFactor = hidden::GROWTHFACTOR;
  if (newSize == container.size)
    return;
  if (container.actual == container.stackdata) {
    if (newSize > container.stacksize) {
      container._switchfromstacktoheap((int)(growthFactor * newSize));
    }
  } else {
    if (newSize == 0) {
      container._switchfromheapstostack();
    } else if (newSize > container.heapsize) {
      container._modifysizeofheap(growthFactor * newSize);
    }
  }
  container.size = newSize;
}

template <typename V, int S, auto P>
void save(auto archiver, seque<V, S, P> const &container) {
  archiver(container.size);
  for (int index = 0; index < container.size; ++index) {
    archiver(container.actual[index]);
  }
}

template <typename V, int S, auto P>
void load(auto archiver, seque<V, S, P> &container) {
  int loadedSize = 0;
  archiver(loadedSize);
  setsize(container, loadedSize);
  for (int index = 0; index < container.size; ++index) {
    archiver(container.actual[index]);
  }
}

template <typename V, int S, auto P>
bool isindexvalid(seque<V, S, P> const &container, int index) {
  return (index < container.size);
}

template <typename V, int S, auto P> void erase(seque<V, S, P> &container) {
  setsize(container, 0);
}

template <typename V, int S, auto P>
void erase(seque<V, S, P> &container, int eraseIndex) {
  if (isindexvalid(container, eraseIndex) && container.size > 0) {
    container.actual[eraseIndex] = container.actual[container.size - 1];
    container.actual[container.size - 1] = V();
    setsize(container, container.size - 1);
  }
}

template <typename V, int S, auto P> void eraselast(seque<V, S, P> &container) {
  if (container.size > 0) {
    setsize(container, container.size - 1);
  }
}

template <typename V, int S, auto P>
void eraseinplace(seque<V, S, P> &container, int startIndex,
                  int numToErase = 1) {
  if (!numToErase || startIndex >= container.size)
    return;
  if (startIndex + numToErase > container.size) {
    numToErase = container.size - startIndex;
  }
  std::move(container.actual + startIndex + numToErase,
            container.actual + container.size, container.actual + startIndex);
  setsize(container, container.size - numToErase);
}

template <typename V, int S, auto P>
void add(seque<V, S, P> &container, int insertIndex, V const &value) {
  container._explodeinvalidindex(insertIndex);
  setsize(container, container.size + 1);
  container[container.size - 1] = container[insertIndex];
  container[insertIndex] = value;
}

template <typename V, int S, auto P>
void addinplace(seque<V, S, P> &container, int insertIndex, V const &value) {
  if (insertIndex > container.size) {
    insertIndex = container.size;
  }
  setsize(container, container.size + 1);
  std::move_backward(container.actual + insertIndex,
                     container.actual + (container.size - 1),
                     container.actual + container.size);
  container[insertIndex] = value;
}

template <typename V, int S, auto P>
void addinplace(seque<V, S, P> &destContainer, int insertIndex,
                seque<V, S, P> const &sourceContainer) {
  if (insertIndex > destContainer.size) {
    insertIndex = destContainer.size;
  }
  destContainer._explodeinvalidindex(insertIndex == 0 ? 0 : (insertIndex - 1));
  const int oldSize = destContainer.size;
  setsize(destContainer, destContainer.size + sourceContainer.size);
  std::move_backward(destContainer.actual + insertIndex,
                     destContainer.actual + oldSize,
                     destContainer.actual + destContainer.size);
  std::copy(destContainer.parallel, sourceContainer.actual,
            sourceContainer.actual + sourceContainer.size,
            destContainer.actual + insertIndex);
}

template <typename V, int S, auto P>
int append(seque<V, S, P> &container, V const &value) {
  setsize(container, container.size + 1);
  container[container.size - 1] = value;
  return container.size - 1;
}

template <typename V, int S, auto P>
int append(seque<V, S, P> &destContainer,
           seque<V, S, P> const &sourceContainer) {
  const int oldSize = destContainer.size;
  setsize(destContainer, destContainer.size + sourceContainer.size);
  std::copy(destContainer.parallel, sourceContainer.actual,
            sourceContainer.actual + sourceContainer.size,
            destContainer.actual + oldSize);
  return oldSize;
}

template <typename V, int S, auto P>
int getsize(seque<V, S, P> const &container) {
  return container.size;
}

template <typename V, int S, auto P>
bool operator<(seque<V, S, P> const &lhs, seque<V, S, P> const &rhs) {
  return std::lexicographical_compare(
      lhs.actual, lhs.actual + lhs.size, rhs.actual, rhs.actual + rhs.size,
      [](auto const &a, auto const &b) { return a < b; });
}

template <typename V, int S, auto P>
bool operator>(seque<V, S, P> const &lhs, seque<V, S, P> const &rhs) {
  return rhs < lhs;
}

template <typename V, int S, auto P>
bool operator>=(seque<V, S, P> const &lhs, seque<V, S, P> const &rhs) {
  return (lhs > rhs) || (lhs == rhs);
}

template <typename V, int S, auto P>
bool operator==(seque<V, S, P> const &lhs, seque<V, S, P> const &rhs) {
  if (lhs.size != rhs.size)
    return false;
  return std::equal(lhs.parallel, lhs.actual, lhs.actual + lhs.size,
                    rhs.actual);
}

template <typename V, int S, auto P>
bool operator!=(seque<V, S, P> const &lhs, seque<V, S, P> const &rhs) {
  return !(lhs == rhs);
}

template <typename V, int S, auto P>
std::ostream &operator<<(std::ostream &os, seque<V, S, P> const &container) {
  auto outputElement = [&](auto element) { os << element << " "; };
  save(outputElement, container);
  return os;
}

template <typename V, int S, auto P>
std::istream &operator>>(std::istream &is, seque<V, S, P> &container) {
  auto inputElement = [&](auto &element) { is >> element; };
  load(inputElement, container);
  return is;
}

template <typename V, int S, auto P>
bool isfullysatisfied(seque<V, S, P> const &container, auto predicate) {
  return std::all_of(container.actual, container.actual + container.size,
                     predicate);
}

template <typename V, int S, auto P>
seque<int, S, P> indicesfromcondition(seque<V, S, P> const &container,
                                      auto predicate) {
  seque<int, S, P> indices;
  auto matchCount = std::count_if(container.parallel, container.actual,
                                  container.actual + container.size, predicate);
  setsize(indices, matchCount);
  int currentIndex = 0;
  for (int pos = 0; pos < container.size; ++pos) {
    if (predicate(container[pos])) {
      indices[currentIndex++] = pos;
    }
  }
  return indices;
}

template <typename V, int S, auto P>
seque<int, S, P> indicesfromvalue(seque<V, S, P> const &container, V value,
                                  seque<int, S, P> const &sortedIndices) {
  int containerSize = container.size;
  if (containerSize == 0) {
    return seque<int, S, P>{};
  }
  int left = 0;
  int right = containerSize - 1;
  seque<int, S, P> indices(containerSize);
  while (left <= right && left < containerSize) {
    int middle = left + (right - left) / 2;
    auto valMid = container[sortedIndices[middle]];
    if (valMid == value) {
      int startIndex = middle;
      while (startIndex > 0 &&
             container[sortedIndices[startIndex - 1]] == value)
        --startIndex;
      int endIndex = middle;
      while (endIndex < containerSize - 1 &&
             container[sortedIndices[endIndex + 1]] == value)
        ++endIndex;
      int matchCount = endIndex - startIndex + 1;
      for (int pos = 0; pos < matchCount; ++pos) {
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

template <typename V, int S, auto P>
seque<int, S, P> indicesfromvalue(seque<V, S, P> const &sortedcontainer,
                                  V value) {
  auto rangeResult =
      std::equal_range(sortedcontainer.actual,
                       sortedcontainer.actual + sortedcontainer.size, value);
  int rangeSize = rangeResult.second - rangeResult.first;
  seque<int, S, P> indices(rangeSize);
  for (int pos = 0; pos < rangeSize; ++pos) {
    indices[pos] =
        static_cast<int>((rangeResult.first - sortedcontainer.actual) + pos);
  }
  return indices;
}

template <typename V, int S, auto P>
seque<int, S, P> getorder(seque<V, S, P> const &container) {
  seque<int, S, P> sortedIndices(container.size);
  std::iota(sortedIndices.actual, sortedIndices.actual + sortedIndices.size, 0);
  std::stable_sort(
      sortedIndices.actual, sortedIndices.actual + sortedIndices.size,
      [&](int idx1, int idx2) { return container[idx1] < container[idx2]; });
  return sortedIndices;
}

template <typename V, int S, auto P>
seque<V, S, P> getasample(seque<V, S, P> const &container, int sampleSize) {
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

template <typename V, int S, auto P>
void setordered(seque<V, S, P> &container) {
  std::stable_sort(container.actual, container.actual + container.size);
}

template <typename V, int S, auto P>
void setordered(seque<V, S, P> &container, auto compareFunc) {
  std::stable_sort(container.actual, container.actual + container.size,
                   compareFunc);
}

template <typename V, int S, auto P>
void applyfunction(seque<V, S, P> const &container, auto func) {
  std::for_each(container.parallel, container.actual,
                container.actual + container.size, func);
}

template <typename V, int S, auto P>
auto getmapped(seque<V, S, P> const &container, auto func) {
  using ReturnType = decltype(func(std::declval<V>()));
  seque<ReturnType, S, P> mappedContainer(container.size);
  std::transform(container.parallel, container.actual,
                 container.actual + container.size, mappedContainer.actual,
                 func);
  return mappedContainer;
}

template <typename V, int S, auto P>
auto getmapped(seque<V, S, P> const &containerA,
               seque<V, S, P> const &containerB, auto func) {
  const int minSize = (std::min)(containerA.size, containerB.size);
  using ReturnType = decltype(func(std::declval<V>(), std::declval<V>()));
  seque<ReturnType, S, P> mappedContainer(minSize);
  std::transform(containerA.parallel, containerA.actual,
                 containerA.actual + minSize, containerB.actual,
                 mappedContainer.actual, func);
  return mappedContainer;
}

template <typename V, int S, auto P>
auto reduce(seque<V, S, P> const &container, auto init, auto binaryFunc) {
  return std::reduce(container.parallel, container.actual,
                     container.actual + container.size, init, binaryFunc);
}

template <typename V, int S, auto P>
auto dotproduct(seque<V, S, P> const &containerA,
                seque<V, S, P> const &containerB, auto init, auto sumFunc,
                auto prodFunc) {
  return std::inner_product(containerA.actual,
                            containerA.actual + containerA.size,
                            containerB.actual, init, sumFunc, prodFunc);
}

template <typename V, int S, auto P>
void setshuffled(seque<V, S, P> &container) {
  std::random_device rd;
  std::mt19937 rng(rd());
  std::shuffle(container.actual, container.actual + container.size, rng);
}

template <typename V, int S, auto P>
void setreversed(seque<V, S, P> &container) {
  std::reverse(container.actual, container.actual + container.size);
}

template <typename V, int S, auto P>
void setrotatedaroundindex(seque<V, S, P> &container, int rotationIndex) {
  if (rotationIndex > container.size) {
    rotationIndex = container.size;
  }
  std::rotate(container.actual, container.actual + rotationIndex,
              container.actual + container.size);
}

template <typename V, int S, auto P>
bool setpermuteclockwise(seque<V, S, P> &container) {
  return std::next_permutation(container.actual,
                               container.actual + container.size);
}

template <typename V, int S, auto P>
bool setpermutecounterclockwise(seque<V, S, P> &container) {
  return std::prev_permutation(container.actual,
                               container.actual + container.size);
}

template <typename V, int S, auto P>
void setunique(seque<V, S, P> &sortedcontainer) {
  auto newEnd = std::unique(sortedcontainer.actual,
                            sortedcontainer.actual + sortedcontainer.size);
  setsize(sortedcontainer, static_cast<int>(newEnd - sortedcontainer.actual));
}

template <typename V, int S, auto P>
void setorderedandunique(seque<V, S, P> &container) {
  setordered(container);
  setunique(container);
}

// Function that returns indices to remove based on duplicates in the sorted
// order.
template <typename V, int S, auto P>
seque<int> getindicesofduplicates(const seque<V, S, P> &array,
                                  const seque<int> &order) {
  // First pass: Count duplicates
  int duplicateCount = 0;
  if (getsize(order) > 0) {
    auto lastVal = array[order[0]];
    for (int i = 1; i < getsize(order); ++i) {
      auto currentVal = array[order[i]];
      if (currentVal == lastVal) {
        ++duplicateCount;
      } else {
        lastVal = currentVal;
      }
    }
  }

  // Allocate space for duplicates
  seque<int> to_remove(duplicateCount);

  // Second pass: Collect duplicate indices
  if (duplicateCount > 0) {
    int index = 0;
    auto lastVal = array[order[0]];
    for (int i = 1; i < getsize(order); ++i) {
      int idx = order[i];
      auto currentVal = array[idx];
      if (currentVal == lastVal) {
        to_remove[index++] = idx;
      } else {
        lastVal = currentVal;
      }
    }
  }

  return to_remove;
}

template <typename V, int S, auto P>
void indicesfromorder(seque<V, S, P> const &sourceContainer,
                      seque<int, S, P> const &sortedIndices,
                      seque<int, S, P> &oldfromnew,
                      seque<int, S, P> &newfromold) {
  int totalSize = getsize(sourceContainer);
  setsize(newfromold, totalSize);
  if (!totalSize) {
    setsize(oldfromnew, 0);
    return;
  }
  int firstOccurrence =
      sortedIndices[0]; // this is the position of the smallest
  newfromold[firstOccurrence] = firstOccurrence;
  for (int index = 1; index < totalSize; ++index) {
    int currentSortedIndex = sortedIndices[index];
    bool sameVal = (sourceContainer[sortedIndices[index]] ==
                    sourceContainer[sortedIndices[index - 1]]);
    newfromold[currentSortedIndex] =
        sameVal ? firstOccurrence : currentSortedIndex;
    if (!sameVal) {
      firstOccurrence = currentSortedIndex;
    }
  }
  int uniqueCount = 0;
  for (int index = 0; index < totalSize; ++index) {
    if (newfromold[index] == index) {
      newfromold[uniqueCount++] = index;
    }
  }
  setsize(newfromold, uniqueCount);
  setsize(oldfromnew, totalSize);
  for (int index = 0; index < totalSize; ++index) {
    oldfromnew[index] = static_cast<int>(-1);
  }
  for (int uniqueIndex = 0; uniqueIndex < uniqueCount; ++uniqueIndex) {
    oldfromnew[newfromold[uniqueIndex]] = uniqueIndex;
  }
  for (int index = 0; index < totalSize; ++index) {
    if (oldfromnew[index] == static_cast<int>(-1)) {
      oldfromnew[index] = oldfromnew[newfromold[index]];
    }
  }
}

template <typename V, int S, auto P>
seque<V, S, P> getintersection(seque<V, S, P> const &sortedcontainerA,
                               seque<V, S, P> const &sortedcontainerB) {
  seque<V, S, P> resultContainer(sortedcontainerA.size + sortedcontainerB.size);
  auto endIt = std::set_intersection(
      sortedcontainerA.actual, sortedcontainerA.actual + sortedcontainerA.size,
      sortedcontainerB.actual, sortedcontainerB.actual + sortedcontainerB.size,
      resultContainer.actual);
  setsize(resultContainer, static_cast<int>(endIt - resultContainer.actual));
  return resultContainer;
}

template <typename V, int S, auto P>
seque<V, S, P> getunion(seque<V, S, P> const &sortedcontainerA,
                        seque<V, S, P> const &sortedcontainerB) {
  seque<V, S, P> resultContainer(sortedcontainerA.size + sortedcontainerB.size);
  auto endIt = std::set_union(
      sortedcontainerA.actual, sortedcontainerA.actual + sortedcontainerA.size,
      sortedcontainerB.actual, sortedcontainerB.actual + sortedcontainerB.size,
      resultContainer.actual);
  setsize(resultContainer, static_cast<int>(endIt - resultContainer.actual));
  return resultContainer;
}

template <typename V, int S, auto P>
seque<V, S, P> getdifference(seque<V, S, P> const &sortedcontainerA,
                             seque<V, S, P> const &sortedcontainerB) {
  seque<V, S, P> resultContainer(sortedcontainerA.size + sortedcontainerB.size);
  auto endIt = std::set_difference(
      sortedcontainerA.actual, sortedcontainerA.actual + sortedcontainerA.size,
      sortedcontainerB.actual, sortedcontainerB.actual + sortedcontainerB.size,
      resultContainer.actual);
  setsize(resultContainer, static_cast<int>(endIt - resultContainer.actual));
  return resultContainer;
}

template <typename V, int S, auto P>
seque<V, S, P> getsymmetricdifference(seque<V, S, P> const &sortedcontainerA,
                                      seque<V, S, P> const &sortedcontainerB) {
  seque<V, S, P> resultContainer(sortedcontainerA.size + sortedcontainerB.size);
  auto endIt = std::set_symmetric_difference(
      sortedcontainerA.actual, sortedcontainerA.actual + sortedcontainerA.size,
      sortedcontainerB.actual, sortedcontainerB.actual + sortedcontainerB.size,
      resultContainer.actual);
  setsize(resultContainer, static_cast<int>(endIt - resultContainer.actual));
  return resultContainer;
}

template <typename V, int S, auto P>
bool getincludessubset(seque<V, S, P> const &sortedsuperset,
                       seque<V, S, P> const &sortedsubset) {
  return std::includes(
      sortedsuperset.actual, sortedsuperset.actual + sortedsuperset.size,
      sortedsubset.actual, sortedsubset.actual + sortedsubset.size);
}

template <typename V, int S, auto P>
int getindextoinsert(seque<V, S, P> const &sortedcontainer, V value) {
  return static_cast<int>(
      std::lower_bound(sortedcontainer.actual,
                       sortedcontainer.actual + sortedcontainer.size, value) -
      sortedcontainer.actual);
}

template <typename V, int S, auto P>
int addretainingorder(seque<V, S, P> &sortedcontainer, V value) {
  int insertPosition = getindextoinsert(sortedcontainer, value);
  addinplace(sortedcontainer, insertPosition, value);
  return insertPosition;
}

#endif
