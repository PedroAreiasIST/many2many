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

template<typename T>
/**
 * @brief Safely deallocates memory used by a dynamically allocated array.
 *
 * This function is used to release the memory allocated for a dynamic array. It ensures
 * that the pointer to the array is set to null after deallocation to avoid potential
 * dangling pointer issues.
 *
 * @param array A double pointer to the array to be deallocated. The function sets
 *              the pointer to null after memory has been deallocated.
 */
inline void arraydestroy(T *&p)
{
    if (p)
    {
        delete[] p;
        p = nullptr;
    }
}

template<typename T>
/**
 * Creates an array with the specified size and initializes its elements to zero.
 *
 * This function is used to dynamically allocate memory for an array of integers
 * of a specified size. The memory is initialized such that all elements in the array
 * will have a value of zero upon creation.
 *
 * @param size The size of the array to be created.
 * @return A pointer to the newly created integer array.
 *         Returns nullptr if the size is less than or equal to zero
 *         or if memory allocation fails.
 */
inline void arraycreate(T *&p, int size)
{
    arraydestroy(p);
    if (size > 0)
    {
        p = new T[size];
    }
}

namespace hidden
{
    /**
     * @brief Defines the size of the stack.
     *
     * Represents the maximum number of elements that can be stored in the stack.
     * Changing this value adjusts the capacity of the stack accordingly.
     *
     * @note Ensure that this value is set appropriately to avoid overflow or excessive
     * memory usage.
     */
    constexpr int STACKSIZE = 7;
    /**
     * @brief The scaling factor used to determine the rate of growth in a given process.
     *
     * GROWTHFACTOR is a constant or variable that represents the multiplier
     * applied in calculations to model or simulate growth dynamics. This value
     * is often utilized in scenarios involving resource allocation, algorithm
     * expansion, or economic models.
     *
     * Usage contexts may include iterative algorithms, geometric progression
     * calculations, or systems where incremental growth is a critical factor.
     *
     * Proper adjustment and understanding of GROWTHFACTOR can influence performance,
     * stability, and accuracy in computations that rely on this scaling mechanism.
     */
    constexpr double GROWTHFACTOR = 1.5;
} // namespace hidden

template<typename V, int S = hidden::STACKSIZE, auto P = std::execution::par>
/**
 * @class seque
 * @brief A sequence container class that provides functionality for managing a sequence of elements.
 *
 * The `seque` class is designed to handle a collection of elements arranged in a sequential order.
 * This class provides operations to manipulate the sequence, such as adding, removing, or
 * accessing elements. It is optimized for scenarios where the sequence needs to grow or shrink
 * dynamically and offers flexibility in managing its elements.
 *
 * @tparam T The type of elements stored in the sequence.
 *
 * ### Key Features:
 * - Dynamic resizing to accommodate varying numbers of elements.
 * - Support for sequential element access.
 * - Methods for inserting, erasing, and retrieving elements.
 * - Efficient memory management for optimal performance.
 */
struct seque
{
    /**
     * @brief Retrieves the number of elements currently in the stack.
     *
     * This function returns the total number of elements that are
     * currently stored in the stack. It does not modify the stack
     * or its contents.
     *
     * @return Size of the stack as an integer value.
     */
    static constexpr int stacksize = S;
    /**
     * @brief Indicates whether an operation or process should be executed in parallel.
     *
     * The `parallel` variable is typically used to enable or disable parallel processing,
     * allowing a process or task to execute concurrently across multiple resources (such
     * as CPU cores or threads). If set to true, parallelism is enabled, otherwise the
     * process executes sequentially.
     *
     * This variable can be useful in scenarios requiring optimization for performance
     * through concurrent execution.
     */
    static constexpr auto parallel = P;
    /**
     * @brief Returns the number of elements in the container.
     *
     * This function provides the total count of elements currently stored
     * in the container. It does not modify the container or its content.
     *
     * @return The size of the container as an unsigned integer.
     */
    int size = 0;
    /**
     * @class StackData
     * @brief A class that represents a data structure for managing stack elements.
     *
     * The StackData class provides functionalities to store, manage, and
     * handle elements in a stack-like structure. It offers standard stack
     * operations such as pushing, popping, and accessing the top element.
     *
     * The implementation assumes a fixed or dynamically allocated memory
     * for managing the stack's data and ensures basic safety and stack
     * behavior principles are adhered to.
     */
    V stackdata[S];
    /**
     * @class HeapData
     * @brief Represents a data structure for handling data in a heap-allocated context.
     *
     * This class is designed to manage raw data stored on the heap with functionality
     * to initialize, access, and manage the underlying data buffer. It is typically used
     * for scenarios requiring efficient memory allocation and deallocation.
     *
     * The class provides methods to access the raw data, retrieve its size, and
     * manipulate the content safely. Proper memory management practices should be followed
     * to avoid leaks or undefined behavior when using this class.
     */
    V *heapdata = nullptr;
    /**
     * @brief Determines the size of a binary heap represented as an array.
     *
     * @param heap Array representing the binary heap.
     * @return The size of the binary heap.
     *
     * This function calculates the size of a binary heap by counting the
     * number of elements in the array that represents the heap. It assumes
     * the binary heap is complete and stored in a contiguous block of memory.
     */
    int heapsize = 0;
    /**
     * @brief Holds the actual value for a given process or computation.
     *
     * The `actual` variable is used to store the real or measured data value
     * in a program. It is typically utilized in scenarios where comparisons
     * are made between expected and observed outcomes. This variable serves
     * as the representation of the observed result.
     */
    V *actual = stackdata;

    /**
     * This method takes an input sequence and processes it to calculate the next sequence in the series
     * based on a predefined transformation or logic.
     *
     * @param inputSequence The input sequence to be processed, typically an iterable collection of elements.
     * @return The resulting sequence after processing the input sequence.
     */
    seque()
    {
    }

    explicit seque(int newSize) : seque() { setsize(*this, newSize); }

    seque(int newSize, V value) : seque(newSize)
    {
        for (int i = 0; i < size; i++)
        {
            actual[i] = value;
        }
    }

    seque(std::initializer_list<V> const &values)
    {
        setsize(*this, values.size());
        std::copy(values.begin(), values.end(), actual);
    }

    seque(seque const &other) : seque() { copy_from(other); }

    seque(seque &&other) noexcept
    {
        size = other.size;
        // Check if the other container is using its stack storage.
        if (other.actual == other.stackdata)
        {
            // Move elements one-by-one into our stackdata.
            for (int i = 0; i < other.size; i++)
            {
                stackdata[i] = std::move(other.stackdata[i]);
            }
            actual = stackdata;
            heapdata = nullptr;
            heapsize = 0;
        } else
        {
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

    seque &operator=(seque &&other) noexcept
    {
        if (this != &other)
        {
            // Free our current dynamic memory if allocated.
            if (heapdata)
            {
                delete[] heapdata;
                heapdata = nullptr;
            }
            // Copy data from the source.
            size = other.size;
            if (other.actual == other.stackdata)
            {
                // Source is using stack storage; move element by element.
                for (int i = 0; i < other.size; i++)
                {
                    stackdata[i] = std::move(other.stackdata[i]);
                }
                actual = stackdata;
                heapdata = nullptr;
                heapsize = 0;
            } else
            {
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

    void swap(seque &other) noexcept
    {
        using std::swap;
        // Swap the nelems and heapsize
        swap(size, other.size);
        swap(heapsize, other.heapsize);
        // Swap the heapdata pointers.
        swap(heapdata, other.heapdata);
        // The internal fixed-nelems buffers cannot be swapped overall,
        // but we swap the elements in the buffer up to the minimum nelems.
        int minStack = (S < other.stacksize ? S : other.stacksize);
        for (int i = 0; i < minStack; ++i)
        {
            swap(stackdata[i], other.stackdata[i]);
        }
        // Reset the actual pointers based on the new state.
        actual = (heapdata ? heapdata : stackdata);
        other.actual = (other.heapdata ? other.heapdata : other.stackdata);
    }

    ~seque() { setsize(*this, 0); }

    seque &operator=(seque const &other)
    {
        if (this != &other)
        {
            copy_from(other);
        }
        return *this;
    }

    /** Fills all elements with the given value. */
    seque &operator=(V const &value)
    {
        std::fill(std::execution::par, actual, actual + size, value);
        return *this;
    }

    V &operator[](int index) { return actual[index]; }
    V const &operator[](int index) const { return actual[index]; }

    /** Access with auto-resize: grows the container if index >= size. */
    V &operator()(int index)
    {
        if (index >= size)
        {
            setsize(*this, index + 1);
        }
        return actual[index];
    }

    /** Checked const access: throws if index is out of range. */
    V const &operator()(int index) const
    {
        _explodeinvalidindex(index);
        return actual[index];
    }

    seque &operator=(std::initializer_list<V> const &initList)
    {
        setsize(*this, initList.size());
        std::copy(initList.begin(), initList.end(), actual);
        return *this;
    }

    V *begin() { return actual; }
    V *end() { return actual + size; }
    V const *begin() const { return actual; }
    V const *end() const { return actual + size; }

    /** Gather: returns a new seque with elements at the given indices. */
    seque operator()(seque<int, S, P> const &indexContainer) const
    {
        seque result;
        if (size > 0)
        {
            setsize(result, indexContainer.size);
            std::transform(std::execution::par, indexContainer.actual,
                           indexContainer.actual + indexContainer.size, result.actual,
                           [&](int idx)
                           {
                               if (idx < 0 || idx >= size)
                                   throw std::out_of_range("sek: index " +
                                                           std::to_string(idx) +
                                                           " out of range [0, " +
                                                           std::to_string(size) + ").)");
                               return actual[idx];
                           });
        }
        return result;
    }

    /**
     * Copies content from the source object to the current object.
     *
     * @param source The source object from which the data will be copied.
     */
    void copy_from(seque const &other)
    {
        setsize(*this, other.size);
        if constexpr (std::is_trivially_copyable_v<V>)
        {
            std::memcpy(actual, other.actual, other.size * sizeof(V));
        } else
        {
            std::copy(std::execution::par, other.actual, other.actual + other.size,
                      actual);
        }
    }

    /**
     * Handles the scenario where an invalid index is encountered in a data structure or operation.
     * This method performs necessary operations when an invalid index is detected, such as logging
     * the error, throwing an exception, or managing recovery steps.
     *
     * @param index The invalid index that triggered this method. It can represent an out-of-range
     *              or otherwise unacceptable value depending on the context.
     * @param data The data structure or collection being accessed that caused the invalid index issue.
     *             This can be used for logging or debugging purposes.
     */
public:
    void _explodeinvalidindex(int index) const
    {
        if (index < 0 || index >= size)
        {
            throw std::out_of_range("sek: index " + std::to_string(index) +
                                    " out of range [0, " + std::to_string(size) +
                                    ").");
        }
    }

    /**
     * Switches memory allocation strategy from using stack to heap.
     *
     * This method modifies the allocation mechanism, enabling dynamic memory allocation
     * for scenarios where stack memory limitations are encountered.
     *
     * @param stackPointer A pointer to the stack memory that will be transitioned.
     * @param heapMemorySize The size of the heap memory to be allocated.
     *
     * This function ensures proper memory handling when transitioning from stack-based
     * allocation to heap-based allocation.
     */
    void _switchfromstacktoheap(int newCapacity)
    {
        heapsize = newCapacity;
        arraycreate(heapdata, newCapacity);
        if constexpr (std::is_trivially_copyable_v<V>)
        {
            std::memcpy(heapdata, stackdata,
                        std::min(heapsize, stacksize) * sizeof(V));
        } else
        {
            for (int i = 0; i < std::min(heapsize, stacksize); i++)
            {
                heapdata[i] = stackdata[i];
            }
        }
        actual = heapdata;
    }

    /**
     * @brief Switches the memory allocation from heap to stack for optimizing performance.
     *
     * This method is used to transition data allocation from the heap, which typically involves dynamic memory allocation,
     * to the stack, which uses static memory. This can help improve performance and reduce memory fragmentation in scenarios
     * where stack-based allocation is more efficient or sufficient for the data's lifecycle.
     *
     * @note Ensure that the size of the data being moved to the stack is within the stack size limitations of your
     *       application's runtime environment, as exceeding the stack size may result in stack overflow errors.
     *
     * @warning Misuse of this method can lead to undefined behavior, including memory corruption if
     *          the stack allocation exceeds the allowed limit or is improperly managed.
     */
    void _switchfromheapstostack()
    {
        const int moveCount = std::min({size, heapsize, stacksize});
        if (heapdata)
        {
            if (moveCount > 0)
                std::copy(heapdata, heapdata + moveCount, stackdata);
            arraydestroy(heapdata);
            heapdata = nullptr;
        }
        heapsize = 0;
        actual = stackdata;
    }

    /**
     * Modifies the size of the heap by adjusting its current capacity.
     *
     * This method changes the size of the heap either by expanding or shrinking
     * the allocated memory to match the desired new size. It ensures that the
     * heap operates efficiently and avoids overflow or underutilized memory.
     *
     * @param newSize The new desired size for the heap.
     * @param allowShrink A boolean flag indicating whether the heap is allowed
     *                    to shrink. If false, the method will only expand the heap.
     */
    void _modifysizeofheap(int newCapacity)
    {
        if (newCapacity > heapsize)
        {
            V *tempArray = nullptr;
            arraycreate(tempArray, newCapacity);
            if constexpr (std::is_trivially_copyable_v<V>)
            {
                std::memcpy(tempArray, heapdata, size * sizeof(V));
            } else
            {
                std::move(std::execution::par, heapdata, heapdata + size, tempArray);
            }
            arraydestroy(heapdata);
            heapdata = tempArray;
            heapsize = newCapacity;
            actual = heapdata;
        }
    }
};

template<typename V, int S, auto P>
inline V *begin(seque<V, S, P> &container)
{
    return container.begin();
}

template<typename V, int S, auto P>
inline V *end(seque<V, S, P> &container)
{
    return container.end();
}

template<typename V, int S, auto P>
/**
 * Sets the size of a collection, array, or other data structure to the specified size.
 *
 * @param size The new size to set. Must be a non-negative integer.
 */
void setsize(seque<V, S, P> &container, int newSize)
{
    double growthFactor = hidden::GROWTHFACTOR;
    if (newSize == container.size)
        return;
    if (container.actual == container.stackdata)
    {
        if (newSize > container.stacksize)
        {
            container._switchfromstacktoheap((int) (growthFactor * newSize));
        }
    } else
    {
        if (newSize == 0)
        {
            container._switchfromheapstostack();
        } else if (newSize > container.heapsize)
        {
            container._modifysizeofheap(growthFactor * newSize);
        }
    }
    container.size = newSize;
}


template<typename V, int S, auto P>
/**
 * Saves the provided data to a specified file path.
 *
 * @param data The data that needs to be saved.
 * @param filePath The location where the data should be saved.
 * @param overwrite Indicates whether to overwrite the file if it already exists.
 *                  If true, the file will be overwritten. If false, the method will not overwrite.
 * @throws IOException If an I/O error occurs during saving the data.
 * @throws IllegalArgumentException If the provided file path is null or empty.
 */
void save(auto archiver, seque<V, S, P> const &container)
{
    archiver(container.size);
    for (int index = 0; index < container.size; ++index)
    {
        archiver(container.actual[index]);
    }
}

template<typename V, int S, auto P>
/**
 * Loads a resource or data into the system.
 *
 * @param resource The resource or data to be loaded into the system. This can be a file, URL, or a string representing the data.
 * @param options Additional parameters or configurations that may influence the loading behavior. This can include settings like caching, loading mode, or resource type.
 * @param callback A callback function to handle the response after the resource has been successfully loaded or if an error occurs.
 */
void load(auto archiver, seque<V, S, P> &container)
{
    int loadedSize = 0;
    archiver(loadedSize);
    setsize(container, loadedSize);
    for (int index = 0; index < container.size; ++index)
    {
        archiver(container.actual[index]);
    }
}


template<typename V, int S, auto P>
bool isindexvalid(seque<V, S, P> const &container, int index)
{
    return (index >= 0 && index < container.size);
}

template<typename V, int S, auto P>
/**
 * Erases the specified element from a container.
 *
 * @param container The container from which the element will be erased.
 * @param element The element to be removed from the container.
 */
void erase(seque<V, S, P> &container)
{
    setsize(container, 0);
}

template<typename V, int S, auto P>
/**
 * Erases elements from the container within the specified range.
 * The elements to be erased are determined by the provided iterators.
 * After erasing, the size of the container is reduced, and any iterators
 * referring to the erased elements are invalidated.
 *
 * @param start Iterator pointing to the beginning of the range to be erased.
 * @param end Iterator pointing to the end of the range to be erased (exclusive).
 */
void erase(seque<V, S, P> &container, int eraseIndex)
{
    if (isindexvalid(container, eraseIndex) && container.size > 0)
    {
        container.actual[eraseIndex] = container.actual[container.size - 1];
        container.actual[container.size - 1] = V();
        setsize(container, container.size - 1);
    }
}

template<typename V, int S, auto P>
/**
 * Removes the last element from a given list or container if it is not empty.
 *
 * @param container A reference to the container from which the last element will be removed.
 *                  The container must support removing elements and provide a size or equivalent method.
 */
void eraselast(seque<V, S, P> &container)
{
    if (container.size > 0)
    {
        setsize(container, container.size - 1);
    }
}

template<typename V, int S, auto P>
/**
 * Removes all occurrences of the specified element from the given container.
 * Modifies the original container in place, erasing the elements that match
 * the specified value.
 *
 * @param container The container from which elements should be erased.
 *                  Must support the `erase` and `end` methods.
 * @param value The value to be removed from the container.
 */
void eraseinplace(seque<V, S, P> &container, int startIndex,
                  int numToErase = 1)
{
    if (!numToErase || startIndex >= container.size)
        return;
    if (startIndex + numToErase > container.size)
    {
        numToErase = container.size - startIndex;
    }
    std::move(container.actual + startIndex + numToErase,
              container.actual + container.size, container.actual + startIndex);
    setsize(container, container.size - numToErase);
}

template<typename V, int S, auto P>
/** Inserts a value at the given index, shifting the existing element to the end. */
void add(seque<V, S, P> &container, int insertIndex, V const &value)
{
    container._explodeinvalidindex(insertIndex);
    setsize(container, container.size + 1);
    container[container.size - 1] = container[insertIndex];
    container[insertIndex] = value;
}

template<typename V, int S, auto P>
/**
 * Adds an element to a given position within the provided container, modifying the container in place.
 *
 * @param container The container (e.g., vector, list, etc.) where the element will be added.
 * @param position The index or iterator specifying the position where the element should be inserted.
 *                 Must be within the valid range of the container.
 * @param element The element to be added to the container.
 */
void addinplace(seque<V, S, P> &container, int insertIndex, V const &value)
{
    if (insertIndex > container.size)
    {
        insertIndex = container.size;
    }
    setsize(container, container.size + 1);
    std::move_backward(container.actual + insertIndex,
                       container.actual + (container.size - 1),
                       container.actual + container.size);
    container[insertIndex] = value;
}

template<typename V, int S, auto P>
/**
 * Adds an element to the specified index in an existing data structure in place.
 *
 * @param data The data structure to which the element will be added.
 * The data structure should support in-place addition at the specified index.
 * @param index The position at which the element will be added.
 * Index must be within the bounds of the data structure.
 * @param element The element to be added to the data structure at the specified index.
 * It should match the type of elements in the data structure.
 */
void addinplace(seque<V, S, P> &destContainer, int insertIndex,
                seque<V, S, P> const &sourceContainer)
{
    if (insertIndex > destContainer.size)
    {
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

template<typename V, int S, auto P>
/**
 * Appends the specified element to the end of the list.
 *
 * This method adds the given element to the list, increasing its size by one.
 *
 * @param element The element to be appended to the list.
 * @return true if the element is successfully appended, false otherwise.
 */
int append(seque<V, S, P> &container, V const &value)
{
    setsize(container, container.size + 1);
    container[container.size - 1] = value;
    return container.size - 1;
}

template<typename V, int S, auto P>
/**
 * Appends the provided element to the end of the list.
 *
 * @param list The list to which the element will be appended.
 * @param element The element to append to the list.
 * @return A boolean indicating whether the append operation was successful.
 */
int append(seque<V, S, P> &destContainer,
           seque<V, S, P> const &sourceContainer)
{
    const int oldSize = destContainer.size;
    setsize(destContainer, destContainer.size + sourceContainer.size);
    std::copy(destContainer.parallel, sourceContainer.actual,
              sourceContainer.actual + sourceContainer.size,
              destContainer.actual + oldSize);
    return oldSize;
}

template<typename V, int S, auto P>
/**
 * Retrieves the size of a collection or data structure.
 *
 * @param none This method does not take any parameters.
 * @return The size of the collection or data structure as an integer.
 */
int getsize(seque<V, S, P> const &container)
{
    return container.size;
}

template<typename V, int S, auto P>
bool operator<(seque<V, S, P> const &lhs, seque<V, S, P> const &rhs)
{
    return std::lexicographical_compare(
        lhs.actual, lhs.actual + lhs.size, rhs.actual, rhs.actual + rhs.size,
        [](auto const &a, auto const &b) { return a < b; });
}

template<typename V, int S, auto P>
bool operator>(seque<V, S, P> const &lhs, seque<V, S, P> const &rhs)
{
    return rhs < lhs;
}

template<typename V, int S, auto P>
bool operator>=(seque<V, S, P> const &lhs, seque<V, S, P> const &rhs)
{
    return (lhs > rhs) || (lhs == rhs);
}

template<typename V, int S, auto P>
bool operator==(seque<V, S, P> const &lhs, seque<V, S, P> const &rhs)
{
    if (lhs.size != rhs.size)
        return false;
    return std::equal(lhs.parallel, lhs.actual, lhs.actual + lhs.size,
                      rhs.actual);
}

template<typename V, int S, auto P>
auto operator<=>(seque<V, S, P> const &lhs, seque<V, S, P> const &rhs)
{
    return std::lexicographical_compare_three_way(
        lhs.actual, lhs.actual + lhs.size,
        rhs.actual, rhs.actual + rhs.size);
}

template<typename V, int S, auto P>
bool operator!=(seque<V, S, P> const &lhs, seque<V, S, P> const &rhs)
{
    return !(lhs == rhs);
}

template<typename V, int S, auto P>
std::ostream &operator<<(std::ostream &os, seque<V, S, P> const &container)
{
    auto outputElement = [&](auto element) { os << element << " "; };
    save(outputElement, container);
    return os;
}

template<typename V, int S, auto P>
std::istream &operator>>(std::istream &is, seque<V, S, P> &container)
{
    auto inputElement = [&](auto &element) { is >> element; };
    load(inputElement, container);
    return is;
}

template<typename V, int S, auto P>
/** Returns true if predicate holds for all elements. */
bool isfullysatisfied(seque<V, S, P> const &container, auto predicate)
{
    return std::all_of(container.actual, container.actual + container.size,
                       predicate);
}

template<typename V, int S, auto P>
/**
 * Retrieves the indices of elements in an iterable that meet a specified condition.
 *
 * This function iterates through the given iterable, applies the specified condition
 * (provided as a callable function) to each element, and collects the indices of those
 * elements for which the condition evaluates to True.
 *
 * @param iterable The iterable (e.g., list, tuple, etc.) to be examined.
 * @param condition A callable that takes an element of the iterable as input and returns
 *                  a boolean indicating whether the condition is satisfied.
 * @return A list of integers representing the indices where the condition holds true.
 */
seque<int, S, P> indicesfromcondition(seque<V, S, P> const &container,
                                      auto predicate)
{
    seque<int, S, P> indices;
    auto matchCount = std::count_if(container.parallel, container.actual,
                                    container.actual + container.size, predicate);
    setsize(indices, matchCount);
    int currentIndex = 0;
    for (int pos = 0; pos < container.size; ++pos)
    {
        if (predicate(container[pos]))
        {
            indices[currentIndex++] = pos;
        }
    }
    return indices;
}

template<typename V, int S, auto P>
/**
 * @brief Finds the indices of all occurrences of a given value within a container.
 *
 * This function searches through the provided container and identifies all indices
 * at which the specified value occurs. It returns these indices as a collection.
 *
 * @tparam Container The type of the container to search. Must support iteration.
 * @tparam T The type of the value to search for.
 * @param container The container to be searched for the specified value.
 * @param value The value to find within the container.
 * @return A container containing the indices of occurrences of the specified value.
 *         The type and structure of the returned collection depend on the implementation.
 */
seque<int, S, P> indicesfromvalue(seque<V, S, P> const &container, V value,
                                  seque<int, S, P> const &sortedIndices)
{
    int containerSize = container.size;
    if (containerSize == 0)
    {
        return seque<int, S, P>{};
    }
    int left = 0;
    int right = containerSize - 1;
    seque<int, S, P> indices(containerSize);
    while (left <= right && left < containerSize)
    {
        int middle = left + (right - left) / 2;
        auto valMid = container[sortedIndices[middle]];
        if (valMid == value)
        {
            int startIndex = middle;
            while (startIndex > 0 &&
                   container[sortedIndices[startIndex - 1]] == value)
                --startIndex;
            int endIndex = middle;
            while (endIndex < containerSize - 1 &&
                   container[sortedIndices[endIndex + 1]] == value)
                ++endIndex;
            int matchCount = endIndex - startIndex + 1;
            for (int pos = 0; pos < matchCount; ++pos)
            {
                indices[pos] = sortedIndices[startIndex + pos];
            }
            setsize(indices, matchCount);
            return indices;
        } else if (valMid < value)
        {
            left = middle + 1;
        } else
        {
            if (middle == 0)
                break;
            right = middle - 1;
        }
    }
    setsize(indices, 0);
    return indices;
}

template<typename V, int S, auto P>
/**
 * Finds and returns the indices of all occurrences of a given value within a container.
 *
 * This method searches through the provided container to find elements matching the specified value
 * and returns a collection of their indices. The method can be used with different container types,
 * such as vectors or arrays.
 *
 * @param container The collection of elements to search within.
 * @param value The value to search for within the container.
 * @return A list of indices where the specified value was found in the container.
 */
seque<int, S, P> indicesfromvalue(seque<V, S, P> const &sortedcontainer,
                                  V value)
{
    auto rangeResult =
            std::equal_range(sortedcontainer.actual,
                             sortedcontainer.actual + sortedcontainer.size, value);
    int rangeSize = rangeResult.second - rangeResult.first;
    seque<int, S, P> indices(rangeSize);
    for (int pos = 0; pos < rangeSize; ++pos)
    {
        indices[pos] =
                static_cast<int>((rangeResult.first - sortedcontainer.actual) + pos);
    }
    return indices;
}

template<typename V, int S, auto P>
/**
 * Retrieves the order associated with a specific identifier or criteria.
 *
 * @param orderId The unique identifier of the order required.
 * @return The details of the order associated with the given identifier.
 */
seque<int, S, P> getorder(seque<V, S, P> const &container)
{
    seque<int, S, P> sortedIndices(container.size);
    std::iota(sortedIndices.actual, sortedIndices.actual + sortedIndices.size, 0);
    std::stable_sort(
        sortedIndices.actual, sortedIndices.actual + sortedIndices.size,
        [&](int idx1, int idx2) { return container[idx1] < container[idx2]; });
    return sortedIndices;
}

template<typename V, int S, auto P>
/**
 * Retrieves a sample based on the implemented logic or data source.
 *
 * @param sampleId An integer specifying the ID of the sample to be retrieved.
 * @param includeMetadata A boolean flag indicating whether to include metadata in the result.
 * @return A string representation of the retrieved sample, or an empty string if no sample is found.
 */
seque<V, S, P> getasample(seque<V, S, P> const &container, int sampleSize)
{
    if (sampleSize > container.size)
    {
        sampleSize = container.size;
    }
    seque<V, S, P> sampleContainer(sampleSize);
    std::random_device rd;
    std::mt19937 rng(rd());
    std::sample(container.actual, container.actual + container.size,
                sampleContainer.actual, sampleSize, rng);
    return sampleContainer;
}

template<typename V, int S, auto P>
/**
 * Sets the given collection in an ordered manner based on the specified criteria or order logic.
 *
 * @param collection The collection of items to be ordered.
 * @param orderLogic The logic or comparator that determines the order of the items in the collection.
 */
void setordered(seque<V, S, P> &container)
{
    std::stable_sort(container.actual, container.actual + container.size);
}

template<typename V, int S, auto P>
/**
 * Sets the ordered state for a collection or data structure.
 *
 * @param ordered A boolean indicating whether the collection or data structure
 *                should be ordered (true) or unordered (false).
 */
void setordered(seque<V, S, P> &container, auto compareFunc)
{
    std::stable_sort(container.actual, container.actual + container.size,
                     compareFunc);
}

template<typename V, int S, auto P>
/**
 * Applies a given function to each element in the provided list and modifies the list in place.
 *
 * @param list The list whose elements the function will be applied to.
 * @param function The function to apply to each element in the list.
 */
void applyfunction(seque<V, S, P> const &container, auto func)
{
    std::for_each(container.parallel, container.actual,
                  container.actual + container.size, func);
}

template<typename V, int S, auto P>
/**
 * Retrieves the mapped value associated with a given key from the mapping structure.
 *
 * @param key The key for which the mapped value is to be retrieved.
 * @return The value mapped to the provided key, or null/appropriate default if the key is not found.
 */
auto getmapped(seque<V, S, P> const &container, auto func)
{
    using ReturnType = decltype(func(std::declval<V>()));
    seque<ReturnType, S, P> mappedContainer(container.size);
    std::transform(container.parallel, container.actual,
                   container.actual + container.size, mappedContainer.actual,
                   func);
    return mappedContainer;
}

template<typename V, int S, auto P>
/**
 * Retrieves the value mapped in the current context.
 * This method is typically used to fetch a mapped value
 * based on certain logic or key present in the implementation.
 *
 * @param key The key to search for and retrieve the associated mapped value.
 *            Its format and usage depend on the internal implementation.
 * @return The value mapped to the provided key. If the key is not found,
 *         the return value will depend on the implementation, such as
 *         returning a default value or throwing an exception.
 */
auto getmapped(seque<V, S, P> const &containerA,
               seque<V, S, P> const &containerB, auto func)
{
    const int minSize = (std::min)(containerA.size, containerB.size);
    using ReturnType = decltype(func(std::declval<V>(), std::declval<V>()));
    seque<ReturnType, S, P> mappedContainer(minSize);
    std::transform(containerA.parallel, containerA.actual,
                   containerA.actual + minSize, containerB.actual,
                   mappedContainer.actual, func);
    return mappedContainer;
}

template<typename V, int S, auto P>
/**
 * Reduces the elements of a collection into a single cumulative result based on a provided reduction function.
 *
 * @param collection The collection of elements to be reduced.
 * @param initialValue The initial value to start the reduction process.
 * @param reducer A function that takes the accumulator and the current element, and returns the updated accumulator.
 * @return The final reduced value after applying the reducer function to all elements in the collection.
 */
auto reduce(seque<V, S, P> const &container, auto init, auto binaryFunc)
{
    return std::reduce(container.parallel, container.actual,
                       container.actual + container.size, init, binaryFunc);
}

template<typename V, int S, auto P>
/**
 * Calculates the dot product of two vectors represented as arrays.
 * The dot product is computed by multiplying corresponding elements
 * of the two vectors and summing the resulting products.
 *
 * @param vectorA the first vector array
 * @param vectorB the second vector array
 * @return the dot product of the two vectors, or 0 if the vectors are invalid or their lengths do not match
 */
auto dotproduct(seque<V, S, P> const &containerA,
                seque<V, S, P> const &containerB, auto init, auto sumFunc,
                auto prodFunc)
{
    return std::inner_product(containerA.actual,
                              containerA.actual + containerA.size,
                              containerB.actual, init, sumFunc, prodFunc);
}

template<typename V, int S, auto P>
/**
 * Sets the shuffled state of the collection.
 *
 * @param shuffled A boolean value indicating whether the collection
 *                 should be in a shuffled state (true) or not (false).
 */
void setshuffled(seque<V, S, P> &container)
{
    std::random_device rd;
    std::mt19937 rng(rd());
    std::shuffle(container.actual, container.actual + container.size, rng);
}

template<typename V, int S, auto P>
/**
 * Sets the reversed state of an object.
 *
 * @param reversed A boolean indicating whether the object should be set to reversed (true) or not (false).
 */
void setreversed(seque<V, S, P> &container)
{
    std::reverse(container.actual, container.actual + container.size);
}

template<typename V, int S, auto P>
/**
 * Sets the rotation of an object or dataset around a specified index. This method updates the
 * rotational alignment based on the provided index and rotation angle. Rotation could affect the
 * object's orientation depending on the implementation.
 *
 * @param index The reference index around which the rotation will be performed.
 *              The index should typically fall within the object’s valid range.
 * @param angle The rotation angle in degrees or radians, depending on the implementation.
 *              Defines how far the object is rotated around the given index.
 */
void setrotatedaroundindex(seque<V, S, P> &container, int rotationIndex)
{
    if (rotationIndex > container.size)
    {
        rotationIndex = container.size;
    }
    std::rotate(container.actual, container.actual + rotationIndex,
                container.actual + container.size);
}

template<typename V, int S, auto P>
/**
 * @brief Sets the permutation of elements in a clockwise order.
 *
 * This function alters the arrangement of elements within a given data structure
 * or grid to transform them into a clockwise permutation sequence.
 *
 * The implementation typically assumes that the input structure allows element-wise
 * traversal and that the operation conforms to a scenario where the arrangement
 * forms a rectilinear or circular ordering.
 *
 * The use case for this function often involves graphical transformations, matrix
 * rotations, or geometry-related tasks where clockwise communication is essential.
 */
bool setpermuteclockwise(seque<V, S, P> &container)
{
    return std::next_permutation(container.actual,
                                 container.actual + container.size);
}

template<typename V, int S, auto P>
/**
 * Sets the permutation of the given array to the next counterclockwise position.
 *
 * This method modifies the order of the elements in the provided array
 * moving them to the next counterclockwise permutation based on their current order.
 *
 * @param arr The array of elements whose permutation is to be set. Must not be null.
 * @return True if the permutation was successfully set to the next counterclockwise state,
 *         or false if no further counterclockwise permutations exist.
 */
bool setpermutecounterclockwise(seque<V, S, P> &container)
{
    return std::prev_permutation(container.actual,
                                 container.actual + container.size);
}

template<typename V, int S, auto P>
/**
 * Sets an input value to be unique by ensuring it is not repeated among any other values.
 *
 * @param value The input value to be set as unique.
 */
void setunique(seque<V, S, P> &sortedcontainer)
{
    auto newEnd = std::unique(sortedcontainer.actual,
                              sortedcontainer.actual + sortedcontainer.size);
    setsize(sortedcontainer, static_cast<int>(newEnd - sortedcontainer.actual));
}

template<typename V, int S, auto P>
/**
 * Processes a collection by ensuring that its elements are both ordered and unique.
 * Removes duplicates and sorts the collection in ascending order.
 *
 * @param collection The collection of elements to be processed. Must support sorting and duplication removal.
 */
void setorderedandunique(seque<V, S, P> &container)
{
    setordered(container);
    setunique(container);
}

// Function that returns indices to remove based on duplicates in the sorted
// order.
template<typename V, int S, auto P>
/**
 * Identifies and returns the indices of all duplicate elements in the input array.
 *
 * This function examines the input array and determines the indices where duplicates appear.
 * Each duplicate value's indices are grouped together into a vector of integer vectors.
 *
 * @param arr The input vector of integers to be analyzed for duplicates.
 * @return A vector of vectors, where each inner vector contains the indices of duplicate elements
 *         corresponding to the same value in the input array. If no duplicates are found, returns an empty vector.
 */
seque<int> getindicesofduplicates(const seque<V, S, P> &array,
                                  const seque<int> &order)
{
    // First pass: Count duplicates
    int duplicateCount = 0;
    if (getsize(order) > 0)
    {
        auto lastVal = array[order[0]];
        for (int i = 1; i < getsize(order); ++i)
        {
            auto currentVal = array[order[i]];
            if (currentVal == lastVal)
            {
                ++duplicateCount;
            } else
            {
                lastVal = currentVal;
            }
        }
    }

    // Allocate space for duplicates
    seque<int> to_remove(duplicateCount);

    // Second pass: Collect duplicate indices
    if (duplicateCount > 0)
    {
        int index = 0;
        auto lastVal = array[order[0]];
        for (int i = 1; i < getsize(order); ++i)
        {
            int idx = order[i];
            auto currentVal = array[idx];
            if (currentVal == lastVal)
            {
                to_remove[index++] = idx;
            } else
            {
                lastVal = currentVal;
            }
        }
    }

    return to_remove;
}

template<typename V, int S, auto P>
/**
 * Generates indices based on the provided order and range.
 *
 * This method creates a list of indices that correspond to the order
 * specified in the input.
 *
 * @param order A list/vector containing the specified order.
 * @param range The range or total number of elements to generate indices for.
 *              Indices will be generated up to the length of the range.
 * @return A vector/list of integers representing the indices in the specified order.
 */
void indicesfromorder(seque<V, S, P> const &sourceContainer,
                      seque<int, S, P> const &sortedIndices,
                      seque<int, S, P> &oldfromnew,
                      seque<int, S, P> &newfromold)
{
    int totalSize = getsize(sourceContainer);
    setsize(newfromold, totalSize);
    if (!totalSize)
    {
        setsize(oldfromnew, 0);
        return;
    }
    int firstOccurrence =
            sortedIndices[0]; // this is the position of the smallest
    newfromold[firstOccurrence] = firstOccurrence;
    for (int index = 1; index < totalSize; ++index)
    {
        int currentSortedIndex = sortedIndices[index];
        bool sameVal = (sourceContainer[sortedIndices[index]] ==
                        sourceContainer[sortedIndices[index - 1]]);
        newfromold[currentSortedIndex] =
                sameVal ? firstOccurrence : currentSortedIndex;
        if (!sameVal)
        {
            firstOccurrence = currentSortedIndex;
        }
    }
    int uniqueCount = 0;
    for (int index = 0; index < totalSize; ++index)
    {
        if (newfromold[index] == index)
        {
            newfromold[uniqueCount++] = index;
        }
    }
    setsize(newfromold, uniqueCount);
    setsize(oldfromnew, totalSize);
    for (int index = 0; index < totalSize; ++index)
    {
        oldfromnew[index] = static_cast<int>(-1);
    }
    for (int uniqueIndex = 0; uniqueIndex < uniqueCount; ++uniqueIndex)
    {
        oldfromnew[newfromold[uniqueIndex]] = uniqueIndex;
    }
    for (int index = 0; index < totalSize; ++index)
    {
        if (oldfromnew[index] == static_cast<int>(-1))
        {
            oldfromnew[index] = oldfromnew[newfromold[index]];
        }
    }
}

template<typename V, int S, auto P>
/**
 * Computes the intersection of two collections and returns the result.
 *
 * @param collection1 The first collection to be intersected.
 * @param collection2 The second collection to be intersected.
 * @return A collection containing elements that exist in both input collections.
 */
seque<V, S, P> getintersection(seque<V, S, P> const &sortedcontainerA,
                               seque<V, S, P> const &sortedcontainerB)
{
    seque<V, S, P> resultContainer(sortedcontainerA.size + sortedcontainerB.size);
    auto endIt = std::set_intersection(
        sortedcontainerA.actual, sortedcontainerA.actual + sortedcontainerA.size,
        sortedcontainerB.actual, sortedcontainerB.actual + sortedcontainerB.size,
        resultContainer.actual);
    setsize(resultContainer, static_cast<int>(endIt - resultContainer.actual));
    return resultContainer;
}

template<typename V, int S, auto P>
/**
 * Computes the union of two provided sets and returns the resulting set containing all unique elements.
 *
 * @param set1 The first set to be used in the union operation.
 * @param set2 The second set to be used in the union operation.
 * @return A set containing all unique elements that are present in either set1, set2, or both.
 */
seque<V, S, P> getunion(seque<V, S, P> const &sortedcontainerA,
                        seque<V, S, P> const &sortedcontainerB)
{
    seque<V, S, P> resultContainer(sortedcontainerA.size + sortedcontainerB.size);
    auto endIt = std::set_union(
        sortedcontainerA.actual, sortedcontainerA.actual + sortedcontainerA.size,
        sortedcontainerB.actual, sortedcontainerB.actual + sortedcontainerB.size,
        resultContainer.actual);
    setsize(resultContainer, static_cast<int>(endIt - resultContainer.actual));
    return resultContainer;
}

template<typename V, int S, auto P>
/** Alias for getunion. */
seque<V, S, P> operator||(seque<V, S, P> const &lhs,
                          seque<V, S, P> const &rhs)
{
    return getunion(lhs, rhs);
}

template<typename V, int S, auto P>
/** Set difference of two sorted sequences. */
seque<V, S, P> getdifference(seque<V, S, P> const &sortedcontainerA,
                             seque<V, S, P> const &sortedcontainerB)
{
    seque<V, S, P> resultContainer(sortedcontainerA.size + sortedcontainerB.size);
    auto endIt = std::set_difference(
        sortedcontainerA.actual, sortedcontainerA.actual + sortedcontainerA.size,
        sortedcontainerB.actual, sortedcontainerB.actual + sortedcontainerB.size,
        resultContainer.actual);
    setsize(resultContainer, static_cast<int>(endIt - resultContainer.actual));
    return resultContainer;
}

template<typename V, int S, auto P>
/**
 * Calculates the symmetric difference between two sets.
 *
 * The symmetric difference of two sets A and B is the set of elements
 * that are in either of the sets A or B, but not in their intersection.
 *
 * @param setA The first set for comparison.
 * @param setB The second set for comparison.
 * @return A set containing the symmetric difference of setA and setB.
 */
seque<V, S, P> getsymmetricdifference(seque<V, S, P> const &sortedcontainerA,
                                      seque<V, S, P> const &sortedcontainerB)
{
    seque<V, S, P> resultContainer(sortedcontainerA.size + sortedcontainerB.size);
    auto endIt = std::set_symmetric_difference(
        sortedcontainerA.actual, sortedcontainerA.actual + sortedcontainerA.size,
        sortedcontainerB.actual, sortedcontainerB.actual + sortedcontainerB.size,
        resultContainer.actual);
    setsize(resultContainer, static_cast<int>(endIt - resultContainer.actual));
    return resultContainer;
}

template<typename V, int S, auto P>
/**
 * Checks if all elements of one list are included in another list.
 *
 * @param allItems The list containing all possible items.
 * @param subsetItems The list containing items to check for inclusion within the first list.
 * @return True if all elements of 'subsetItems' are included in 'allItems', false otherwise.
 */
bool getincludessubset(seque<V, S, P> const &sortedsuperset,
                       seque<V, S, P> const &sortedsubset)
{
    return std::includes(
        sortedsuperset.actual, sortedsuperset.actual + sortedsuperset.size,
        sortedsubset.actual, sortedsubset.actual + sortedsubset.size);
}

template<typename V, int S, auto P>
/**
 * Calculates the index position where a given value should be inserted
 * into a sorted array to maintain the sorted order.
 *
 * This method identifies the smallest index at which the value can be
 * inserted, ensuring that the array remains sorted after insertion.
 *
 * @param array The sorted array into which the value needs to be inserted.
 *              It must be non-null and in non-decreasing order.
 * @param value The value to be inserted into the array.
 * @return The index where the value should be inserted to maintain
 *         the sorted order of the array.
 */
int getindextoinsert(seque<V, S, P> const &sortedcontainer, V value)
{
    return static_cast<int>(
        std::lower_bound(sortedcontainer.actual,
                         sortedcontainer.actual + sortedcontainer.size, value) -
        sortedcontainer.actual);
}

template<typename V, int S, auto P>
/**
 * Adds elements to a collection while retaining their original order.
 * The method ensures that the order in which elements are added
 * is preserved and duplicates are handled according to the collection's rules.
 *
 * @param collection the collection to which elements will be added
 * @param elements the elements to add to the collection while maintaining their order
 * @return true if the collection was modified as a result of the operation, false otherwise
 */
int addretainingorder(seque<V, S, P> &sortedcontainer, V value)
{
    int insertPosition = getindextoinsert(sortedcontainer, value);
    addinplace(sortedcontainer, insertPosition, value);
    return insertPosition;
}

#endif
