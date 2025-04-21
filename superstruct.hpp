#ifndef GODOFTYPES_HPP
#define GODOFTYPES_HPP
#include "basics.hpp"
#include "seque.hpp"
#include <initializer_list>
#include <tuple>
#include <utility> // for std::forward

template <class T, class Tuple> struct indexoftype {};

template <class T, class... Types>
struct indexoftype<T, std::tuple<T, Types...>> {
  static constexpr std::size_t value = 0;
};

template <class T, class U, class... Types>
struct indexoftype<T, std::tuple<U, Types...>> {
  static const std::size_t value =
      1 + indexoftype<T, std::tuple<Types...>>::value;
};

/**
 * @brief The godstruct class, registers the lists of entities in use
 * at compile time
 */
template <typename... T> struct superstruct {
  // content
  std::tuple<seque<T>...> as;
  // std::tuple type (aka Tuple)
  using Tuple = std::tuple<T...>;
  // size of type list
  static constexpr auto Size = sizeof...(T);
  // give Nth type
  template <std::size_t N>
  using Nth = typename std::tuple_element<N, Tuple>::type;
  // give First type
  using First = Nth<0>;
  // give Last type
  using Last = Nth<Size - 1>;

  friend std::ostream &operator<<(std::ostream &os,
                                  superstruct<T...> const &s) {
    std::ios_base::sync_with_stdio(false);
    auto lambda = [&](auto const &arg) { os << arg << " "; };
    tupleforcycle(s.as, lambda);
    return os;
  }

  friend std::istream &operator>>(std::istream &is, superstruct<T...> &s) {
    std::ios_base::sync_with_stdio(false);
    auto lambda = [&](auto const &arg) { is >> arg; };
    tupleforcycle(s.as, lambda);
    return is;
  }
};

template <typename T> seque<T> &getsequence(auto &tp) noexcept {
  return std::get<seque<T>>(tp.as);
}

template <typename T, typename G> constexpr size_t getnumber() {
  return indexoftype<T, typename G::Tuple>::value;
}

template <typename T,      // the element type you want to append
          typename Mesh,   // the type of your mesh
          typename... Args // the arguments you’d pass to T’s constructor
          >
int appendnode(Mesh &mesh, Args &&...args) {
  auto &seq = getsequence<T>(mesh);
  return append(seq, T{std::forward<Args>(args)...});
}

// ────────────────────────────────────────────────────────────────────────────
// 1) Variadic‑args overload
//    call like: append_relation<Foo,Bar>(myTM, myMatrix, a, b, c);
// ────────────────────────────────────────────────────────────────────────────
template <typename TypeManager, // your manager type
          typename ElemType,    // your element type (e.g. Foo)
          typename NodeType,    // your node type    (e.g. Bar)
          typename MatrixGetter // e.g. decltype(myMatrix)
          >
int appendelement(MatrixGetter &matrixGetter,
                  std::initializer_list<int> fields = {}) {
  // 1) look up the two indices
  int elemIdx = getnumber<ElemType, TypeManager>();
  int nodeIdx = getnumber<NodeType, TypeManager>();
  // 3) construct a new ElemType and append it
  return appendelement(matrixGetter, elemIdx, nodeIdx, fields);
}

#endif
