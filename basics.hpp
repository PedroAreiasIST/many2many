#pragma once

#include <algorithm>
#include <boost/core/demangle.hpp>
#include <cstddef>
#include <fstream>
#include <iostream>
#include <random>
#include <sstream> // Needed for std::ostringstream const& stringfromvalue
#include <string>
#include <tabulate/latex_exporter.hpp>
#include <tuple>
#include <typeinfo>
#include <utility>
#include <vector>
#include "pfr_non_boost-master/include/pfr.hpp"
using namespace tabulate;
using Row_t = Table::Row_t;


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
 * @brief Safely deletes a dynamically allocated array.
 *
 * @tparam T Array element type.
 * @param p Pointer to the array to delete. After deletion, p is set to nullptr.
 */
template<typename T>
inline void arraydestroy(T *&p) noexcept
{
    if (p)
    {
        delete[] p;
        p = nullptr;
    }
}

/**
 * @brief Creates an array of a given size, destroying any previously allocated memory.
 *
 * @tparam T Array element type.
 * @param p Pointer to store the new array.
 * @param size Number of elements const& the new array.
 */
template<typename T>
inline void arraycreate(T *&p, size_t size)
{
    arraydestroy(p);
    if (size > 0)
    {
        p = new T[size];
        std::fill(p, p + size, T());
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
template<typename A>
void latextableexport(A const &arr, size_t nrows, size_t ncols)
{
    Table movies;
    Row_t firstrow(ncols + 1);
    for (int i = 1; i < ncols + 1; ++i)
    {
        std::string temp = stringfromvalue(i);
    }

    movies.add_row(Row_t{"S/N", "Movie Name", "Director", "Estimated Budget", "Release Date"});
    movies.add_row(Row_t{"tt1979376", "Toy Story 4", "Josh Cooley", "$200,000,000", "21 June 2019"});
    movies.add_row(Row_t{"tt3263904", "Sully", "Clint Eastwood", "$60,000,000", "9 September 2016"});
    movies.add_row(Row_t{"tt1535109", "Captain Phillips", "Paul Greengrass", "$55,000,000", " 11 October 2013"});

    // center align 'Director' column
    movies.column(2).format().font_align(FontAlign::center);

    // right align 'Estimated Budget' column
    movies.column(3).format().font_align(FontAlign::right);

    // right align 'Release Date' column
    movies.column(4).format().font_align(FontAlign::right);

    // Color header cells
    for (size_t i = 0; i < 5; ++i)
    {
        movies[0][i].format().font_color(Color::white).font_style({FontStyle::bold}).background_color(Color::blue);
    }

    LatexExporter exporter;
    exporter.configure().indentation(8);
    auto latex = exporter.dump(movies);

    // tabulate::table
    std::cout << movies << "\n\n";

    // Exported Markdown
    std::cout << latex << std::endl;
}
