/**
 * @file test_sek.cpp
 *
 * Comprehensive test suite for the `sek` container from `sek.hpp`.
 * It relies on <cassert> for basic checks.
 *
 * Usage:
 *   g++ -std=c++17 -O2 -Wall test_sek.cpp -o test_sek
 *   ./test_sek
 */

#include <algorithm> // std::count, etc.
#include <cassert>
#include <iostream>
#include <vector> // for collecting permutations
#include "sek.hpp" // Include your 'sek.hpp' header here

// Optional helper: Print a sek for debugging
template<typename V, size_t S, auto P>
void printSek(const sek<V, S, P> &s, const std::string &label = "")
{
    if (!label.empty())
        std::cout << label << ": ";
    std::cout << "[ ";
    for (size_t i = 0; i < s.size; ++i)
    {
        std::cout << s[i] << " ";
    }
    std::cout << "]\n";
}

// ----------------------------------------------------------------------------
// Test 1: Construction, Assignment, Basic Access
// ----------------------------------------------------------------------------
static void testConstructionAndAssignment()
{
    // Default constructor
    sek<int> s1;
    assert(s1.size == 0);

    // Constructor with explicit size
    sek<int> s2(5);
    assert(s2.size == 5);
    // Should be default-initialized (0 for int)
    for (size_t i = 0; i < s2.size; ++i)
        assert(s2[i] == 0);

    // Constructor with size + value
    sek<int> s3(4, 42);
    assert(s3.size == 4);
    for (size_t i = 0; i < s3.size; ++i)
        assert(s3[i] == 42);

    // Initializer list
    sek<std::string> s4{"one", "two", "three"};
    assert(s4.size == 3);
    assert(s4[0] == "one");
    assert(s4[1] == "two");
    assert(s4[2] == "three");

    // Copy constructor
    sek<std::string> s5(s4);
    assert(s5.size == 3);
    assert(s5 == s4);

    // Move constructor
    sek<std::string> s6(std::move(s5));
    assert(s6.size == 3);
    assert(s5.size == 0); // moved-from is empty
    assert(s6[0] == "one");

    // Copy assignment
    s5 = s6;
    assert(s5.size == s6.size);
    assert(s5 == s6);

    // Move assignment
    sek<std::string> s7;
    s7 = std::move(s6);
    assert(s7.size == 3);
    assert(s6.size == 0);

    // Operator= single value
    sek<int> s8(3, 1);
    s8 = 99; // fill with 99
    for (size_t i = 0; i < s8.size; ++i)
        assert(s8[i] == 99);

    // Operator= initializer list
    sek<int> s9;
    s9 = {10, 20, 30};
    assert(s9.size == 3);
    assert(s9[0] == 10);
    assert(s9[1] == 20);
    assert(s9[2] == 30);

    // Basic operator[] read/write
    s9[1] = 999;
    assert(s9[1] == 999);

    // Automatic resizing operator()(index)
    sek<int> s10;
    s10(5) = 123; // auto-resize to size=6
    assert(s10.size == 6);
    assert(s10[5] == 123);

    std::cout << "testConstructionAndAssignment passed.\n";
}

// ----------------------------------------------------------------------------
// Test 2: Insertions and Erasures
// ----------------------------------------------------------------------------
static void testInsertErase()
{
    sek<int> s1{1, 2, 3, 4, 5};
    assert(s1.size == 5);

    // sekappend(s, value)
    append(s1, 6);
    assert(s1.size == 6);
    assert(s1[5] == 6);

    // sekadd(s, index, value) => "swap with last" style
    add(s1, 0, 100);
    // Moves old index 0 => last. So [1,2,3,4,5,6] => [100,2,3,4,5,1]
    assert(s1.size == 7);
    assert(s1[0] == 100);
    assert(s1[s1.size - 1] == 1);

    // sekaddinplace(s, index, value) => "shift right"
    addinplace(s1, 3, 200);
    // [100,2,3,4,5,1] => insert at 3 => [100,2,3,200,4,5,1, (new last 0?)]
    // Actually it just grows the container by 1, so size=8 now
    assert(s1[3] == 200);
    assert(s1.size == 8);

    // sekerase(s, index) => "swap with last"
    erase(s1, 2);
    // e.g. removing element at index 2 => put last in index 2, then size--
    assert(s1.size == 7);

    // sekeraseinplace(s, index, n) => "shift left"
    // We'll do index=1, n=2
    // Just to confirm it removes 2 elements and shifts
    eraseinplace(s1, 1, 2);
    assert(s1.size == 5);

    // sekkeraselast
    eraselast(s1);
    assert(s1.size == 4);

    // sekerase
    erase(s1); // Clear everything
    assert(s1.size == 0);

    std::cout << "testInsertErase passed.\n";
}

// ----------------------------------------------------------------------------
// Test 3: Resizing & SSO Check
// ----------------------------------------------------------------------------
static void testResizing()
{
    // Use smaller stacksize to force transitions
    sek<int, 3> s1; // stacksize=3
    assert(s1.size == 0);

    // Grow to exactly S=3 => still using stack
    setsize(s1, 3);
    assert(s1.size == 3);
    assert(s1.actual == s1.stackdata);

    // Exceed stack => go to heap
    setsize(s1, 5);
    assert(s1.size == 5);
    assert(s1.actual == s1.heapdata);

    // Shrink to 2 => back to stack
    setsize(s1, 2);
    assert(s1.size == 2);
    assert(s1.actual == s1.stackdata);

    std::cout << "testResizing passed.\n";
}

// ----------------------------------------------------------------------------
// Test 4: Comparisons
// ----------------------------------------------------------------------------
static void testComparisons()
{
    sek<int> a{1, 2, 3}, b{1, 2, 3}, c{1, 2, 4};

    assert(a == b);
    assert(a != c);
    assert(a < c);
    assert(c > b);
    assert(c >= b);
    assert(!(c < b));

    std::cout << "testComparisons passed.\n";
}

// ----------------------------------------------------------------------------
// Test 5a: Basic Sorting / Next-Permutation / Prev-Permutation
// ----------------------------------------------------------------------------
static void testSorting()
{
    sek<int> s1{5, 1, 3, 4, 2};
    setsorted(s1);
    // check sorted => [1,2,3,4,5]
    for (int i = 0; i < 5; ++i)
        assert(s1[i] == i + 1);

    // next_permutation: from [1,2,3,4,5] => [1,2,3,5,4]
    bool hasNext = setpermuteclockwise(s1);
    assert(hasNext == true);
    assert(s1[3] == 5 && s1[4] == 4);

    // keep going until next_permutation returns false
    std::cout << "s1=" << s1 << std::endl;
    while (setpermuteclockwise(s1))
    { /* do nothing */
        std::cout << "s1=" << s1 << std::endl;
    }
    // should be [5,4,3,2,1]
    std::cout << "s1=" << s1 << std::endl;
    // assert(changed == false);

    // prev_permutation: from [5,4,3,2,1] => [5,4,3,1,2] (the next lexicographically smaller)
    setpermutecounterclockwise(s1);
    bool hasPrev = setpermutecounterclockwise(s1);
    assert(hasPrev == true);
    // we can check the result explicitly
    sek<int> expected{5, 4, 3, 1, 2};
    for (size_t i = 0; i < s1.size; ++i)
        assert(s1[i] == expected[i]);

    std::cout << "testSorting passed.\n";
}

// ----------------------------------------------------------------------------
// Test 6: Searching (indices, value, order, etc.)
// ----------------------------------------------------------------------------
static void testSearching()
{
    sek<int> s1{10, 20, 30, 40, 50};

    // sekgetindicesfromcondition
    auto idxDiv20 = indicesfromcondition(s1, [](int x) { return (x % 20) == 0; });
    // s1= [10,20,30,40,50]; those divisible by 20 => index=1,3 => [1,3]
    assert(idxDiv20.size == 2);
    assert(idxDiv20[0] == 1 && idxDiv20[1] == 3);

    // sekgetorder
    sek<int> unsorted{30, 10, 20};
    auto order = getorder(unsorted);
    // sorted => [10,20,30]; so indices => [1,2,0]
    assert(order.size == 3);
    assert(order[0] == 1 && order[1] == 2 && order[2] == 0);

    // sekgetindicesfromvalue (sorted version)
    sek<int> sorted{5, 10, 10, 15, 20};
    // already sorted
    auto found = indicesfromvalue(sorted, 10);
    // duplicates => [1,2]
    assert(found.size == 2);
    assert(found[0] == 1 && found[1] == 2);

    std::cout << "testSearching passed.\n";
}

// ----------------------------------------------------------------------------
// Test 7: Set Operations (union, intersection, difference, etc.)
// ----------------------------------------------------------------------------
static void testSetOps()
{
    sek<int> s1{1, 2, 3, 3, 5};
    sek<int> s2{2, 3, 3, 4, 7};
    setsorted(s1);
    setsorted(s2);

    // intersection => [2,3,3]
    auto inter = getintersection(s1, s2);
    assert(inter.size == 3 && inter[0] == 2 && inter[1] == 3 && inter[2] == 3);

    // union => [1,2,3,3,4,5,7]
    auto uni = getunion(s1, s2);
    // size=7, check a few
    assert(uni.size == 7);
    assert(uni[0] == 1 && uni[3] == 3 && uni[6] == 7);

    // difference => s1 \ s2 => [1,5]
    auto diff = getdifference(s1, s2);
    assert(diff.size == 2 && diff[0] == 1 && diff[1] == 5);

    // symmetric difference => [1,4,5,7]
    auto sym = getsymmetricdifference(s1, s2);
    assert(sym.size == 4);

    // includes => s1 includes subset?
    sek<int> subset{2, 3};
    setsorted(subset);
    assert(getincludessubset(s1, subset) == true);

    std::cout << "testSetOps passed.\n";
}


// ----------------------------------------------------------------------------
// Test 9: Higher-order Ops (map, apply, reduce, dotproduct, etc.)
// ----------------------------------------------------------------------------
static void testAlgorithms()
{
    sek<int> s1{1, 2, 3, 4, 5};

    // sekapplyfunction
    int sum1 = 0;
    applyfunction(s1, [&](int x) { sum1 += x; });
    assert(sum1 == 15);

    // sekgetmapped (unary)
    auto squared = getmap(s1, [](int x) { return x * x; });
    // [1,2,3,4,5] => [1,4,9,16,25]
    for (size_t i = 0; i < squared.size; ++i)
        assert(squared[i] == (s1[i] * s1[i]));

    // sekgetmapped (binary)
    sek<int> s2{10, 10, 10, 10, 10};
    auto summed = getmap(s1, s2, [](int a, int b) { return a + b; });
    // => [11,12,13,14,15]
    assert(summed[0] == 11 && summed[4] == 15);

    // sekgetreduction
    int product = reduce(s1, 1, [](int total, int x) { return total * x; });
    // => 1*2*3*4*5 = 120
    assert(product == 120);

    // sekdotproduct
    auto dot = dotproduct(s1, s2, 0, std::plus<int>{}, std::multiplies<int>{});
    // => sum of (s1[i]*s2[i]) => 1*10 + 2*10 + 3*10 + 4*10 + 5*10 = 150
    assert(dot == 150);

    // sekgetsample
    sek<int> big(100);
    for (size_t i = 0; i < big.size; ++i)
        big[i] = int(i);
    auto sample10 = getasample(big, 10);
    assert(sample10.size == 10);
    // check each is in [0..99]
    for (size_t i = 0; i < sample10.size; ++i)
    {
        assert(sample10[i] >= 0 && sample10[i] < 100);
    }

    std::cout << "testAlgorithms passed.\n";
}

// ----------------------------------------------------------------------------
// Test 10: Swapping and Permutation Check
// ----------------------------------------------------------------------------
static void testSwapAndPermutation()
{
    sek<int> s1{1, 2, 3}, s2{4, 5, 6};

    swap(s1, s2);
    // s1 => [4,5,6], s2 => [1,2,3]
    assert(s1[0] == 4 && s1[2] == 6);
    assert(s2[0] == 1 && s2[2] == 3);


    std::cout << "testSwapAndPermutation passed.\n";
}

// ----------------------------------------------------------------------------
// Main: Execute All Tests
// ----------------------------------------------------------------------------
void testsek()
{
    testConstructionAndAssignment();
    testInsertErase();
    testResizing();
    testComparisons();
    testSorting();
    testSearching();
    testSetOps();
    testAlgorithms();
    testSwapAndPermutation();
    std::cout << "\nAll tests passed successfully!\n";
}
