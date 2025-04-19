#ifndef TYPseque_H
#define TYPseque_H
/**
 * @brief The typseque class
 */
template<typename...>
struct typseque
{
};

namespace
{
    /**
     * @brief Merge type sequences
     */
    template<typename AA, typename BB>
    struct typmerge2;

    template<typename... As, typename... Bs>
    struct typmerge2<typseque<As...>, typseque<Bs...> >
    {
        using result_type = typseque<As..., Bs...>;
    };

    template<typename First, typename... Rest>
    struct typsequemerge
    {
        using result_type = typename typmerge2<
            First, typename typsequemerge<Rest...>::result_type>::result_type;
    };

    template<typename One>
    struct typsequemerge<One>
    {
        using result_type = One;
    };
} // namespace
/**
 * @brief Type of any Merge any number of sequences
 */
template<typename... ST>
using typsequemergetype = typename typsequemerge<ST...>::result_type;

/**
 * @brief Pass parameter pack to a struct
 */
namespace
{
    template<typename ST, template <typename...> typename S>
    struct typsequetostruct;

    template<typename... ST, template <typename...> typename S>
    struct typsequetostruct<typseque<ST...>, S>
    {
        using result_type = S<ST...>;
    };
} // namespace

template<typename ST, template <typename...> typename S>
using typsequetostructtype = typename typsequetostruct<ST, S>::result_type;

#endif // TYPseque_H
