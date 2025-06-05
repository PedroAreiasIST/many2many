#ifndef TYPseque_H
#define TYPseque_H
/**
 * @brief The typseque class
 */
template<typename...>
/**
 * @struct typseque
 * @brief Represents a type sequence container.
 *
 * This struct acts as a container for a sequence of types, enabling type-based
 * meta-programming operations such as merging sequences, transforming sequences,
 * and converting the sequence into other structures.
 */
struct typseque
{
};

namespace
{
    /**
     * @brief Merge type sequences
     */
    template<typename AA, typename BB>
    /**
     * @struct typmerge2
     * @brief A template structure used for merging two `typseque` type sequences.
     *
     * This structure provides functionality to concatenate two `typseque` type sequences,
     * resulting in a single type sequence containing all the types from both input sequences.
     *
     * @tparam AA The first `typseque` to merge.
     * @tparam BB The second `typseque` to merge.
     *
     * The result of merging AA and BB is made accessible through the `result_type` alias,
     * which is a new `typseque` containing the combined type lists of AA and BB.
     */
    struct typmerge2;

    template<typename... As, typename... Bs>
    /**
     * @struct typmerge2
     * @brief A struct template to merge two typseque type sequences.
     *
     * This struct takes two typseque type sequences as parameters and produces
     * a new typseque type sequence that merges the types of both sequences.
     * The resulting type sequence combines all the types from the first and
     * second sequences in their respective order.
     *
     * @tparam As... The parameter pack for the first typseque type sequence.
     * @tparam Bs... The parameter pack for the second typseque type sequence.
     *
     * @typedef result_type
     * The resulting typseque type sequence that combines As... and Bs... types.
     */
    struct typmerge2<typseque<As...>, typseque<Bs...> >
    {
        using result_type = typseque<As..., Bs...>;
    };

    template<typename First, typename... Rest>
    /**
     * @brief A metafunction for merging multiple type sequences into a single type sequence.
     *
     * This struct recursively merges multiple `typseque` type sequences by combining the elements
     * within them into a single `typseque` type sequence. It makes use of the `typmerge2` struct
     * to merge two type sequences at a time.
     *
     * It works by:
     * - Taking the first type sequence in the parameter pack (`First`) and recursively merging it
     *   with the result of merging the rest of the type sequences (`Rest...`).
     * - This recursion continues until only one type sequence remains, which is then returned as the
     *   `result_type`.
     *
     * @tparam First The first type sequence in the parameter pack.
     * @tparam Rest The remaining type sequences to be merged.
     *
     * The `result_type` is a `typseque` that combines all elements of the input type sequences.
     */
    struct typsequemerge
    {
        using result_type = typename typmerge2<
            First, typename typsequemerge<Rest...>::result_type>::result_type;
    };

    template<typename One>
    /**
     * @brief A specialized template structure for handling a single type sequence.
     *
     * This specialization of the `typsequemerge` template is designed to work with
     * a single type sequence. When only one type sequence is provided, this specialization
     * sets the `result_type` to be the same as the given type sequence.
     *
     * @tparam One The single type sequence to be handled by this specialization.
     */
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
    /**
     * @struct typsequetostruct
     *
     * @brief A helper struct template used to transform a type sequence into a single structured type.
     *
     * This struct template is utilized to convert a typseque type sequence into a resultant
     * structure by applying a template alias or type. It uses a specialization that works
     * with the `typseque` type and converts it into an instantiation of the provided type
     * template with the types from the sequence.
     *
     * @tparam ST Represents the type sequence wrapped within `typseque`.
     * @tparam S  The template to apply to the types within the sequence.
     */
    struct typsequetostruct;

    template<typename... ST, template <typename...> typename S>
    /**
     * @brief A template struct that transforms a `typseque` instance containing types
     *        into a templated structure of user-defined type.
     *
     * The `typsequetostruct` structure specializes on a `typseque` template
     * containing a variadic set of types (`ST...`) and a user-defined
     * template structure (`S`). It provides an alias `result_type` that
     * instantiates the user-defined structure `S` with the types in `typseque`.
     *
     * @tparam ST Variadic template types packed into `typseque`.
     * @tparam S Template structure to be instantiated with the types.
     *
     * @note This struct is a metafunction used for type transformation at compile time.
     */
    struct typsequetostruct<typseque<ST...>, S>
    {
        using result_type = S<ST...>;
    };
} // namespace

template<typename ST, template <typename...> typename S>
using typsequetostructtype = typename typsequetostruct<ST, S>::result_type;

#endif // TYPseque_H
