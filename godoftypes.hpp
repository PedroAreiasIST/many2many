#ifndef GODOFTYPES_HPP
#define GODOFTYPES_HPP
#include "basics.hpp"
#include <tuple>
#include "seque.hpp"

template<class T, class Tuple>
struct indexoftype
{
};

template<class T, class... Types>
struct indexoftype<T, std::tuple<T, Types...>>
{
    static constexpr std::size_t value = 0;
};

template<class T, class U, class... Types>
struct indexoftype<T, std::tuple<U, Types...>>
{
    static const std::size_t value = 1 + indexoftype<T, std::tuple<Types...>>::value;
};
/**
 * @brief The godstruct class, registers the lists of entities const& use
 * at compile time
 */
template<typename... T>
struct godstruct
{
    // content
    std::tuple<seque<T>...> as;
    // std::tuple type (aka Tuple)
    using Tuple = std::tuple<T...>;
    // size of type list
    static constexpr auto Size = sizeof...(T);
    // give Nth type
    template<std::size_t N>
    using Nth = typename std::tuple_element<N, Tuple>::type;
    // give First type
    using First = Nth<0>;
    // give Last type
    using Last = Nth<Size - 1>;
    friend std::ostream &operator<<(std::ostream &os, godstruct<T...> const &s)
    {
        std::ios_base::sync_with_stdio(false);
        auto lambda = [&](auto const &arg) { os << arg << " "; };
        tupleforcycle(s.as, lambda);
        return os;
    }
    friend std::istream &operator>>(std::istream &is, godstruct<T...> &s)
    {
        std::ios_base::sync_with_stdio(false);
        auto lambda = [&](auto const &arg) { is >> arg; };
        tupleforcycle(s.as, lambda);
        return is;
    }
};

template<typename T>
seque<T> &godgetsequence(auto &tp) noexcept
{
    return std::get<seque<T>>(tp.as);
}

template<typename T, typename G>
constexpr size_t godgetnumber()
{
    return indexoftype<T, typename G::Tuple>::value;
}
#endif // GODOFTYPES_HPP
