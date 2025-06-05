#pragma once

//#include "cereal/archives/json.hpp"
//#include "cereal/cereal.hpp"
//#include "cereal/macros.hpp"
#include "pfr_non_boost-master/include/pfr.hpp"
#include <algorithm>
#include <boost/core/demangle.hpp>
#include <cstddef>
#include <fstream>
#include <iostream>
#include <random>
#include <set>
#include <map>
#include <sstream>
#include <string>
#include <tuple>
#include <typeinfo>
#include <utility>
#include <vector>

template<typename... T>
/**
 * Reads multiple values from the standard input and assigns them to the provided arguments.
 * This function takes a parameter pack and uses fold expression to extract inputs.
 *
 * @tparam T The types of the arguments to be read.
 * @param args The variables into which input values from standard input will be stored*/
inline void readscreen(T &... args)
{
    ((std::cin >> args), ...);
}

template<typename... T>
/**
 *
 */
inline void writescreen(T &&... args)
{
    ((std::cout << args << " "), ...);
    std::cout << std::endl;
}

template<typename T>
/**
 * @brief Safely destroys a pointer by deleting its target and setting it to nullptr.
 *
 * This function deletes the object pointed to by the given pointer and ensures
 * that the pointer is set to nullptr to avoid dangling references. It is safe to
 * call this function with a null pointer.
 *
 * @tparam T The type of the object being pointed to.
 * @param p A reference to a pointer that points to the object to be destroyed.
 *          If `p` is null, the function does nothing.
 */
inline void pointerdestroy(T *&p) noexcept
{
    if (p)
    {
        delete p;
        p = nullptr;
    }
}

template<typename T>
/**
 * @brief Retrieves the demangled name of the type T.
 *
 * @return A string representing the demangled name of the type T.
 */
inline std::string typetoname()
{
    const char *name = typeid(T).name();
    return boost::core::demangle(name);
}

template<typename T>
/**
 * Converts the type of an object to its demangled type name string.
 *
 * @param obj The object whose type name needs to be determined.
 * @return A string containing the demangled type name of the object.
 */
inline std::string typetoname(const T &obj)
{
    const char *name = typeid(obj).name();
    return boost::core::demangle(name);
}

template<typename TupleT, typename Fn>
/**
 * Applies the provided callable object on each element of the given tuple.
 *
 * The function iterates through the elements of the tuple and calls the given
 * callable object with each element as an argument.
 *
 * @param tp*/
inline void tupleforcycle(TupleT &&tp, Fn &&fn)
{
    std::apply(
        [&fn](auto &&... args) { (fn(std::forward<decltype(args)>(args)), ...); },
        std::forward<TupleT>(tp));
}

/**
 * Checks if a file exists and is accessible.
 *
 * @param filename The path to the file to check.
 * @return True if the file exists and can be opened, false otherwise.
 */
inline bool fileifexists(const std::string &filename)
{
    std::ifstream infile(filename);
    return infile.good();
}

/**
 * Opens an output file stream for writing.
 *
 * @param stream Reference to the output file stream to be opened.
 * @param filename Pointer to a C-style string containing the name of the file to open.
 */
inline void fileopenoutput(std::ofstream &stream, const char *filename)
{
    stream.open(filename, std::ios::out);
}

/**
 * Opens a file in input mode.
 *
 * @param stream The input file stream to be opened.
 * @param filename The name of the file to be opened.
 */
inline void fileopeninput(std::ifstream &stream, const char *filename)
{
    stream.open(filename, std::ios::in);
}

template<typename T>
/**
 * @brief Converts a given value of any type to its string representation.
 *
 * This method uses a stringstream to generate the string representation
 * of the input value.
 *
 * @tparam T The type of the value to be converted to a string.
 * @param value The input value to be converted to a string.
 * @return A string representing the input value.
 */
inline std::string stringfromvalue(const T &value)
{
    std::ostringstream oss;
    oss << value;
    return oss.str();
}

/**
 * Executes a system command in the command line.
 *
 * @param command A string representing the command to be executed.
 * @return An integer representing the return value of the system*/
inline int commandinvoke(const std::string &command)
{
    return system(command.c_str());
}

/**
 *
 */
inline std::vector<std::string> commandargget(int argc, char *argv[])
{
    return std::vector<std::string>(argv, argv + argc);
}

template<typename StructType>
/**
 * @class pfrgetallnames
 *
 * @brief This class is responsible for retrieving and managing all names associated
 *        with a specific operation or dataset within a given context.
 *
 * The pfrgetallnames class is designed to obtain, store, and potentially manipulate
 * a collection of names related to a particular source or process. This class can
 * serve as a utility for organizing and accessing the names, which might be
 * fetched from external sources such as databases, configuration files, or APIs.
 *
 * Key functionality includes:
 * -*/
struct pfrgetallnames
{
    /**
     * @brief A collection or list of names.
     *
     * This variable typically holds a group of strings representing individual names.
     * It can be used for a variety of purposes, such as managing user names, identifying persons,
     * or storing any relevant set of name-related data.
     */
    static std::vector<std::string> names;

    template<int I = 0>
    /**
     * Adds a new item to the inventory.
     *
     * @param itemID An integer representing the unique identifier for the item.
     * @param itemName A string representing the name of the item to be added.
     * @param quantity An integer specifying the number of items to add to the inventory.
     */
    static void item(std::vector<std::string> &lst)
    {
        lst.push_back(std::string(pfr::get_name<I, StructType>()));
        if constexpr (I + 1 < pfr::tuple_size<StructType>::value)
        {
            item<I + 1>(lst);
        }
    }
};

template<typename StructType>
/**
 *
 */
std::vector<std::string> pfrgetallnames<StructType>::names;

#define PFRALLNAMES(sname, nomestodos) pfrgetallnames<sname>::item<>(nomestodos)

template<typename T1, typename T2>
/**
 * Overloads the operator for the specified functionality.
 *
 * @param lhs The left-hand side operand of the operator.
 * @param rhs The right-hand side operand of the operator.
 * @return The result of the operation performed by the overloaded operator.
 */
std::ostream &operator<<(std::ostream &out, const std::pair<T1, T2> &p)
{
    out << '(' << p.first << ", " << p.second << ')';
    return out;
}

template<typename T1, typename T2>
/**
 * Overloads the operator to perform a specific operation between two objects or values.
 * The operation can vary depending on the implementation within the class or structure.
 *
 * @param lhs The left-hand side operand involved in the operation.
 * @param rhs The right-hand side operand involved in the operation.
 * @return The result of the operation as a value, determined by the specific implementation.
 */
std::istream &operator>>(std::istream &in, std::pair<T1, T2> &p)
{
    char ch1, ch2, ch3;
    in >> ch1 >> p.first >> ch2 >> p.second >> ch3;
    if (ch1 != '(' || ch2 != ',' || ch3 != ')')
    {
        in.setstate(std::ios::failbit);
    }
    return in;
}

template<typename T, std::size_t N>
/**
 * Overloads the operator for a custom implementation.
 *
 * @param other The object or value to compare or operate with.
 * @return The result of the operation as determined by the custom implementation.
 */
std::ostream &operator<<(std::ostream &out, const std::array<T, N> &arr)
{
    out << '[';
    if (N > 0)
    {
        for (std::size_t i = 0; i < N - 1; ++i)
        {
            out << arr[i] << ", ";
        }
        out << arr[N - 1];
    }
    out << ']';
    return out;
}

template<typename T, std::size_t N>
/**
 * Overloads the operator to perform a specific operation between two objects of the class.
 *
 * @param other The object of the class to be operated with the current instance.
 * @return The result of the operation*/
std::istream &operator>>(std::istream &in, std::array<T, N> &arr)
{
    char ch;
    in >> ch;
    if (ch != '[')
    {
        in.setstate(std::ios::failbit);
        return in;
    }

    for (std::size_t i = 0; i < N; ++i)
    {
        in >> arr[i];
        if (i < N - 1)
        {
            in >> ch;
            if (ch != ',')
            {
                in.setstate(std::ios::failbit);
                return in;
            }
        }
    }

    in >> ch;
    if (ch != ']')
    {
        in.setstate(std::ios::failbit);
    }
    return in;
}

#define _PP_NARG(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14,  \
    _15, _16, N, ...)                                             \
  N
#define PP_NARG(...)                                                           \
  _PP_NARG(__VA_ARGS__, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, \
           0)

// 2) FOR_EACH implementations for 1…8 args
#define _FE_1(WHAT, X) WHAT(X)
#define _FE_2(WHAT, X, ...) WHAT(X), _FE_1(WHAT, __VA_ARGS__)
#define _FE_3(WHAT, X, ...) WHAT(X), _FE_2(WHAT, __VA_ARGS__)
#define _FE_4(WHAT, X, ...) WHAT(X), _FE_3(WHAT, __VA_ARGS__)
#define _FE_5(WHAT, X, ...) WHAT(X), _FE_4(WHAT, __VA_ARGS__)
#define _FE_6(WHAT, X, ...) WHAT(X), _FE_5(WHAT, __VA_ARGS__)
#define _FE_7(WHAT, X, ...) WHAT(X), _FE_6(WHAT, __VA_ARGS__)
#define _FE_8(WHAT, X, ...) WHAT(X), _FE_7(WHAT, __VA_ARGS__)
#define _FE_9(WHAT, X, ...) WHAT(X), _FE_8(WHAT, __VA_ARGS__)
#define _FE_10(WHAT, X, ...) WHAT(X), _FE_9(WHAT, __VA_ARGS__)
#define _FE_11(WHAT, X, ...) WHAT(X), _FE_10(WHAT, __VA_ARGS__)
#define _FE_12(WHAT, X, ...) WHAT(X), _FE_11(WHAT, __VA_ARGS__)
#define _FE_13(WHAT, X, ...) WHAT(X), _FE_12(WHAT, __VA_ARGS__)
#define _FE_14(WHAT, X, ...) WHAT(X), _FE_13(WHAT, __VA_ARGS__)
#define _FE_15(WHAT, X, ...) WHAT(X), _FE_14(WHAT, __VA_ARGS__)
#define _FE_16(WHAT, X, ...) WHAT(X), _FE_15(WHAT, __VA_ARGS__)

// 3) Dispatch FOR_EACH
#define _FOR_EACH_CHOOSER(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12,   \
    _13, _14, _15, _16, NAME, ...)                       \
  NAME
#define FOR_EACH(WHAT, ...)                                                    \
  _FOR_EACH_CHOOSER(__VA_ARGS__, _FE_16, _FE_15, _FE_14, _FE_13, _FE_12,       \
                    _FE_11, _FE_10, _FE_9, _FE_8, _FE_7, _FE_6, _FE_5, _FE_4,  \
                    _FE_3, _FE_2, _FE_1)                                       \
  (WHAT, __VA_ARGS__)

// 4) Wrap it all up
#define REFLECT(ClassName, ...)                                                \
 public: auto operator<=>(ClassName const &) const = default;                         \
 public: template <class Archive> void serialize(Archive &ar) {                       \
    ar(FOR_EACH(CEREAL_NVP, __VA_ARGS__));                                     \
  }
