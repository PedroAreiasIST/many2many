#ifndef TYPSEK_H
#define TYPSEK_H
/**
 * @brief The typsek class
 */
template<typename...>
struct typsek
{};

namespace {
/**
 * @brief Merge type sequences
 */
template<typename AA, typename BB>
struct typmerge2;
template<typename... As, typename... Bs>
struct typmerge2<typsek<As...>, typsek<Bs...>>
{
    using result_type = typsek<As..., Bs...>;
};
template<typename First, typename... Rest>
struct typsekmerge
{
    using result_type =
        typename typmerge2<First, typename typsekmerge<Rest...>::result_type>::result_type;
};
template<typename One>
struct typsekmerge<One>
{
    using result_type = One;
};
}
/**
 * @brief Type of any Merge any number of sequences
 */
template <typename... ST>
using typsekmergetype = typename typsekmerge<ST...>::result_type;

/**
 * @brief Pass parameter pack to a struct
 */
namespace{
template <typename ST,template<typename...> typename S> struct typsektostruct;

template<typename... ST,template<typename...> typename S> struct typsektostruct<typsek<ST...>, S>
{
    using result_type = S<ST...>;
};
}

template <typename ST, template<typename...> typename S>
using typsektostructtype = typename typsektostruct<ST, S>::result_type;

#endif // TYPSEK_H
