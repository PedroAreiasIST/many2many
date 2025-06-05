#ifndef GODOFTYPES_HPP
#define GODOFTYPES_HPP
#include "basics.hpp"
#include "seque.hpp"
#include <initializer_list>
#include <tuple>
#include <utility> // for std::forward

template<class T, class Tuple>
/**
 * @struct indexoftype
 * @brief A utility trait to calculate the zero-based index of a type `T` within a given tuple type.
 *
 * This structure is intended to provide a way to determine the position of a specified type `T`
 * in a tuple through template metaprogramming. If the specified type is not found within the tuple,
 * compilation may fail.
 *
 * @tparam T The type to locate within the tuple.
 * @tparam Tuple The tuple type in which to search for the type `T`.
 */
struct indexoftype
{
};

template<class T, class... Types>
/**
 * @brief A specialization of indexoftype for when the type T is the first type
 *        in a tuple.
 *
 * This template struct determines the index of type T within a given
 * std::tuple. For this specialization, the type T is found at index 0
 * because it matches the first type in the tuple.
 *
 * @tparam T The type to be located within the tuple.
 * @tparam Types The remaining types in the tuple.
 */
struct indexoftype<T, std::tuple<T, Types...> >
{
    /**
     * Represents the index of a specified type T within a tuple where T is the first type.
     *
     * This constant expression is a compile-time value that is always 0 because
     * the type T is located at the very beginning of the tuple.
     */
    static constexpr std::size_t value = 0;
};

template<class T, class U, class... Types>
/**
 * @brief A template specialization of `indexoftype` that calculates the index of
 *        the first occurrence of a specified type `T` within a `std::tuple`.
 *
 * @tparam T The type to search for within the tuple.
 * @tparam U The current type being compared at the head of the tuple.
 * @tparam Types The rest of the types in the tuple to be processed recursively.
 *
 * This struct inherits from a recursive implementation of `indexoftype`. It adds 1
 * to the index value until the specified type `T` is located within the tuple.
 *
 * Note: It is assumed that the type `T` exists within the tuple; otherwise, this
 *       recursive template will fail to find a match.
 */
struct indexoftype<T, std::tuple<U, Types...> >
{
    /**
     * Represents the computed index of a type `T` within a tuple of types `Types...`.
     *
     * This value is calculated recursively by advancing through the tuple
     * and incrementing the index until the type `T` is located. If `T`
     * exists within the tuple, this constant will hold a zero-based index of `T`.
     *
     * @note The static member `value` is intended to be a compile-time
     * constant, facilitating template metaprogramming operations.
     */
    static const std::size_t value =
            1 + indexoftype<T, std::tuple<Types...> >::value;
};

/**
 * @brief The godstruct class, registers the lists of entities in use
 * at compile time
 */
template<typename... T>
/**
 *
 */
struct superstruct
{
    /**
     * @brief Represents a variable for storing a specific value or data.
     *
     * The purpose of the variable `as` is contextual and depends
     * on the design and implementation of the program. Ensure the
     * variable's name and usage are intuitive and meaningful for
     * its intended function.
     */
    std::tuple<seque<T>...> as;
    // std::tuple type (aka Tuple)
    using Tuple = std::tuple<T...>;
    /**
     * @brief Represents the size of an object or structure.
     *
     * The Size variable is used to define or store the dimensions or magnitude
     * of a certain entity, typically in terms of physical or abstract quantity.
     * It can be applied to a variety of concepts such as file sizes, physical dimensions,
     * or any relevant metric where size-related information is required.
     *
     * Ensure that the value assigned to Size is consistent with the context in which it is used.
     * It should be properly validated to avoid anomalies or undesired behavior.
     *
     * @note Proper units or measurement systems (if applicable) should be clearly
     * documented in the context where this variable is utilized.
     */
    static constexpr auto Size = sizeof...(T);
    // give Nth type
    template<std::size_t N>
    using Nth = typename std::tuple_element<N, Tuple>::type;
    // give First type
    using First = Nth<0>;
    // give Last type
    using Last = Nth<Size - 1>;

    /**
     * Overload of the operator to perform a specific operation.
     *
     * @param lhs The left-hand side operand of the operator.
     * @param rhs The right-hand side operand of the operator.
     * @return The result of the operation performed by the operator.
     */
    friend std::ostream &operator<<(std::ostream &os,
                                    superstruct<T...> const &s)
    {
        std::ios_base::sync_with_stdio(false);
        auto lambda = [&](auto const &arg) { os << arg << " "; };
        tupleforcycle(s.as, lambda);
        return os;
    }

    /**
     *
     */
    friend std::istream &operator>>(std::istream &is, superstruct<T...> &s)
    {
        std::ios_base::sync_with_stdio(false);
        auto lambda = [&](auto const &arg) { is >> arg; };
        tupleforcycle(s.as, lambda);
        return is;
    }
};

template<typename T>
/**
 * Retrieves a reference to the seque<T> object stored in the provided tuple-like structure.
 *
 * @param tp A reference to a tuple-like structure containing the seque<T> object.
 * @return A reference to the seque<T> object within the tuple-like structure.
 */
seque<T> &getsequence(auto &tp) noexcept
{
    return std::get<seque<T> >(tp.as);
}

template<typename T, typename G>
/**
 * Retrieves a numeric value from the system or a calculation process.
 *
 * This method is designed to return a specific numeric result calculated
 * or fetched based on the implementation details.
 *
 * @return An integer representing the retrieved or calculated number.
 */
constexpr size_t getnumber()
{
    return indexoftype<T, typename G::Tuple>::value;
}

template<typename T, // the element type you want to append
    typename Mesh, // the type of your mesh
    typename... Args // the arguments you’d pass to T’s constructor
>
/**
 * Appends a new node to the sequence associated with the given mesh.
 *
 * @tparam T The type of object to be appended to the sequence.
 * @tparam Args The types of arguments to forward to the constructor of type T.
 * @param mesh The mesh object that contains the sequence to which the node will be appended.
 * @param args The arguments to be forwarded to the constructor of the new node.
 * @return An integer representing the position or identifier of the newly appended node.
 */
int appendnode(Mesh &mesh, Args &&... args)
{
    auto &seq = getsequence<T>(mesh);
    return append(seq, T{std::forward<Args>(args)...});
}

// ────────────────────────────────────────────────────────────────────────────
// 1) Variadic‑args overload
//    call like: append_relation<Foo,Bar>(myTM, myMatrix, a, b, c);
// ────────────────────────────────────────────────────────────────────────────
template<typename TypeManager, // your manager type
    typename ElemType, // your element type (e.g. Foo)
    typename NodeType, // your node type    (e.g. Bar)
    typename MatrixGetter // e.g. decltype(myMatrix)
>
/**
 * Appends an element to the end of a given container.
 *
 * @param container The container to which the element will be appended.
 *                  It must support push_back or an equivalent operation.
 * @param element The element to append to the container.
 * @return void
 */
int appendelement(MatrixGetter &matrixGetter,
                  std::initializer_list<int> fields = {})
{
    // 1) look up the two indices
    int elemIdx = getnumber<ElemType, TypeManager>();
    int nodeIdx = getnumber<NodeType, TypeManager>();
    // 3) construct a new ElemType and append it
    return appendelement(matrixGetter, elemIdx, nodeIdx, fields);
}

#endif
