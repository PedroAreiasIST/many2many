#pragma once

#include "cereal/archives/json.hpp"
#include "cereal/cereal.hpp"
#include "cereal/macros.hpp"
#include "pfr_non_boost-master/include/pfr.hpp"
#include <algorithm>
#include <boost/core/demangle.hpp>
#include <cstddef>
#include <fstream>
#include <iostream>
#include <random>
#include <set>
#include <sstream>
#include <string>
#include <tuple>
#include <typeinfo>
#include <utility>
#include <vector>

template <typename... T> inline void readscreen(T &...args) {
  ((std::cin >> args), ...);
}

template <typename... T> inline void writescreen(T &&...args) {
  ((std::cout << args << " "), ...);
  std::cout << std::endl;
}

template <typename T> inline void pointerdestroy(T *&p) noexcept {
  if (p) {
    delete p;
    p = nullptr;
  }
}

template <typename T> inline std::string typetoname() {
  const char *name = typeid(T).name();
  return boost::core::demangle(name);
}

template <typename T> inline std::string typetoname(const T &obj) {
  const char *name = typeid(obj).name();
  return boost::core::demangle(name);
}

template <typename TupleT, typename Fn>
inline void tupleforcycle(TupleT &&tp, Fn &&fn) {
  std::apply(
      [&fn](auto &&...args) { (fn(std::forward<decltype(args)>(args)), ...); },
      std::forward<TupleT>(tp));
}

inline bool fileifexists(const std::string &filename) {
  std::ifstream infile(filename);
  return infile.good();
}

inline void fileopenoutput(std::ofstream &stream, const char *filename) {
  stream.open(filename, std::ios::out);
}

inline void fileopeninput(std::ifstream &stream, const char *filename) {
  stream.open(filename, std::ios::in);
}

template <typename T> inline std::string stringfromvalue(const T &value) {
  std::ostringstream oss;
  oss << value;
  return oss.str();
}

inline int commandinvoke(const std::string &command) {
  return system(command.c_str());
}

inline std::vector<std::string> commandargget(int argc, char *argv[]) {
  return std::vector<std::string>(argv, argv + argc);
}

template <typename StructType> struct pfrgetallnames {
  static std::vector<std::string> names;

  template <int I = 0> static void item(std::vector<std::string> &lst) {
    lst.push_back(std::string(pfr::get_name<I, StructType>()));
    if constexpr (I + 1 < pfr::tuple_size<StructType>::value) {
      item<I + 1>(lst);
    }
  }
};

template <typename StructType>
std::vector<std::string> pfrgetallnames<StructType>::names;

#define PFRALLNAMES(sname, nomestodos) pfrgetallnames<sname>::item<>(nomestodos)

template <typename T1, typename T2>
std::ostream &operator<<(std::ostream &out, const std::pair<T1, T2> &p) {
  out << '(' << p.first << ", " << p.second << ')';
  return out;
}

template <typename T1, typename T2>
std::istream &operator>>(std::istream &in, std::pair<T1, T2> &p) {
  char ch1, ch2, ch3;
  in >> ch1 >> p.first >> ch2 >> p.second >> ch3;
  if (ch1 != '(' || ch2 != ',' || ch3 != ')') {
    in.setstate(std::ios::failbit);
  }
  return in;
}

template <typename T, std::size_t N>
std::ostream &operator<<(std::ostream &out, const std::array<T, N> &arr) {
  out << '[';
  if (N > 0) {
    for (std::size_t i = 0; i < N - 1; ++i) {
      out << arr[i] << ", ";
    }
    out << arr[N - 1];
  }
  out << ']';
  return out;
}

template <typename T, std::size_t N>
std::istream &operator>>(std::istream &in, std::array<T, N> &arr) {
  char ch;
  in >> ch;
  if (ch != '[') {
    in.setstate(std::ios::failbit);
    return in;
  }

  for (std::size_t i = 0; i < N; ++i) {
    in >> arr[i];
    if (i < N - 1) {
      in >> ch;
      if (ch != ',') {
        in.setstate(std::ios::failbit);
        return in;
      }
    }
  }

  in >> ch;
  if (ch != ']') {
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
  auto operator<=>(ClassName const &) const = default;                         \
  template <class Archive> void serialize(Archive &ar) {                       \
    ar(FOR_EACH(CEREAL_NVP, __VA_ARGS__));                                     \
  }
