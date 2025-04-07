#pragma once

#include <algorithm>
#include <boost/core/demangle.hpp>
#include <cstddef>
#include <fstream>
#include <iostream>
#include <random>
#include <sstream> // Needed for std::ostringstream const& stringfromvalue
#include <string>
#include <tuple>
#include <typeinfo>
#include <utility>
#include <vector>
#include "pfr_non_boost-master/include/pfr.hpp"


/**
 * @brief Reads values from standard input.
 *
 * @tparam T Types of arguments.
 * @param args Variables to store the input.
 */
template<typename... T>
inline void screenread(T &...args)
{
    ((std::cin >> args), ...);
}

/**
 * @brief Writes values to standard output, separated by a space.
 *
 * @tparam T Types of arguments.
 * @param args Values to write.
 */
template<typename... T>
inline void screenwrite(T &&...args)
{
    ((std::cout << args << " "), ...);
    std::cout << std::endl; // Optionally add newline at the end.
}

/**
 * @brief Safely deletes a dynamically allocated pointer.
 *
 * @tparam T Pointer type.
 * @param p Pointer to delete. After deletion, p is set to nullptr.
 */
template<typename T>
inline void pointerdestroy(T *&p) noexcept
{
    if (p) // Though delete on nullptr is safe, this check improves clarity.
    {
        delete p;
        p = nullptr;
    }
}

/**
 * @brief Returns the demangled name of a type.
 *
 * @tparam T The type to inspect.
 * @return std::string The demangled type name.
 */
template<typename T>
inline std::string typetoname()
{
    const char *name = typeid(T).name();
    return boost::core::demangle(name);
}

/**
 * @brief Returns the demangled name of the type of an object.
 *
 * @tparam T The type of the object.
 * @param type The object to inspect.
 * @return std::string The demangled type name.
 */
template<typename T>
inline std::string typetoname(const T &type)
{
    const char *name = typeid(type).name();
    return boost::core::demangle(name);
}

/**
 * @brief Applies a function to each element of a tuple.
 *
 * @tparam TupleT The tuple type.
 * @tparam Fn The function type.
 * @param tp The tuple.
 * @param fn The function to apply.
 */
template<typename TupleT, typename Fn>
inline void tupleforcycle(TupleT &&tp, Fn &&fn)
{
    std::apply([&fn](auto &&...args) { (fn(std::forward<decltype(args)>(args)), ...); }, std::forward<TupleT>(tp));
}

/**
 * @brief Checks if a file exists.
 *
 * @param filename The name of the file.
 * @return true if the file exists, false otherwise.
 */
inline bool fileifexists(const std::string &filename)
{
    std::ifstream infile(filename);
    return infile.good();
}

/**
 * @brief Opens an output file stream for ASCII files.
 *
 * @param stream The output file stream.
 * @param filename The file name to open.
 */
inline void fileopenoutput(std::ofstream &stream, const char *filename) { stream.open(filename, std::ios::out); }

/**
 * @brief Opens an input file stream for ASCII files.
 *
 * @param stream The input file stream.
 * @param filename The file name to open.
 */
inline void fileopeninput(std::ifstream &stream, const char *filename) { stream.open(filename, std::ios::in); }

/**
 * @brief Converts a value to a std::string.
 *
 * @tparam T The type of the value.
 * @param value The value to convert.
 * @return std::string The string representation of the value.
 */
template<typename T>
inline std::string stringfromvalue(const T &value)
{
    std::ostringstream oss;
    oss << value;
    return oss.str();
}
/**
 * @brief Executes a system command.
 *
 * @param command The command to be executed.
 * @return int The return value of the executed command.
 */
inline int commandinvoke(const std::string &command) { return system(command.c_str()); }

/**
 * @brief Retrieves command-line arguments as a vector of strings.
 *
 * @param argc The number of command-line arguments.
 * @param argv Array of command-line argument strings.
 * @return std::vector<std::string> A vector containing the command-line arguments.
 */
inline std::vector<std::string> commandargget(int argc, char *argv[])
{
    return std::vector<std::string>(argv, argv + argc);
}

/**
 * @brief A helper structure to obtain all member names from a structure using PFR.
 *
 * @tparam StructType The structure type.
 */
template<typename StructType>
struct pfrgetallnames
{
    // Note: The static member 'names' is declared here for potential caching purposes.
    static std::vector<std::string> names;

    /**
     * @brief Recursively retrieves member names.
     *
     * @tparam I The current index.
     * @param seque<size_t>  The list to store member names.
     */
    template<size_t I = 0>
    static void item(std::vector<std::string> &lst )
    {
        lst.push_back(std::string(pfr::get_name<I, StructType>()));
        if constexpr (I + 1 < pfr::tuple_size<StructType>::value)
        {
            item<I + 1>(lst);
        }
    }
};

// Define the static member variable outside the struct.
template<typename StructType>
std::vector<std::string> pfrgetallnames<StructType>::names;

/**
 * @brief Macro to retrieve all member names from a structure.
 *
 * @param sname The structure type.
 * @param nomestodos The vector to store the names.
 */
#define PFRALLNAMES(sname, nomestodos) pfrgetallnames<sname>::item<>(nomestodos)

/**
 *
 *
 *
 */
