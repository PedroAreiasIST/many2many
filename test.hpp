#pragma once
#include <cassert>
#include <cstddef>
#include <iostream>
#include <string>
#include "sek.hpp"
// Helper Function Prototype
template<typename V, size_t S, auto P>
void printSek(const sek<V, S, P> &s, const std::string &label = "");
// Function Prototypes for Tests
void testConstructionAndAssignment();
void testInsertErase();
void testResizing();
void testsek();
