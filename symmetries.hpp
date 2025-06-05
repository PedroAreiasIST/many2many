#ifndef SYMMETRIES_HPP
#define SYMMETRIES_HPP

#include <algorithm>
#include <stdexcept>
#include "seque.hpp"

namespace hidden
{
    using Permutation = seque<int>;
    using Group = seque<Permutation>;
    /**
     * @brief Constant representing an uninitialized or unset state in various data structures or algorithms.
     *
     * This value is often used to indicate an uninitialized slot in arrays or sequences
     * or as a placeholder in situations where no valid value has yet been assigned.
     */
    static const int UNSET = -1;

    /**
     * Checks if the given sequence is a valid full permutation present in the group.
     *
     * The function iterates through the permutations in the group and checks
     * if any of them match the provided sequence exactly.
     *
     * @param position The sequence of integers to validate as a full permutation.
     * @param G The group containing permutations to compare against.
     * @return true if the sequence matches a full permutation in the group, false otherwise.
     */
    inline bool isvalidfullpermutation(const seque<int> &position, const Group &G)
    {
        const int posSize = getsize(position);
        for (const auto &perm: G)
        {
            if (getsize(perm) != posSize)
                continue;

            bool match = true;
            for (int i = 0; i < posSize; ++i)
            {
                if (perm[i] != position[i])
                {
                    match = false;
                    break;
                }
            }
            if (match)
            {
                return true; // Found a matching permutation in the group
            }
        }
        return false;
    }

    /**
     * Checks whether the given sequence can be extended consistently with the provided group of permutations.
     *
     * The method iterates through each permutation in the group and checks if the given sequence `position`
     * matches consistently for indices up to `upTo`. If at least one permutation is consistent, the sequence
     * can be extended, and the method returns true.
     *
     * @param position The current sequence of integers to validate against the group of permutations.
     * @param upTo The index up to which the consistency of the sequence is checked.
     * @param G The group of permutations to be checked against the current sequence.
     * @return Returns true if the sequence can be extended consistently with at least one permutation, false otherwise.
     */
    inline bool canextend(const seque<int> &position, int upTo, const Group &G)
    {
        for (const auto &perm: G)
        {
            if (getsize(perm) != getsize(position))
                continue;

            bool match = true;
            // Check consistency up to 'upTo' index
            for (int k = 0; k <= upTo; ++k)
            {
                if (position[k] != UNSET && position[k] != perm[k])
                {
                    match = false;
                    break;
                }
            }
            if (match)
            {
                return true; // At least one permutation is consistent so far
            }
        }
        return false;
    }

    /**
     * Recursively attempts to assign slots in a permutation, ensuring the permutation
     * satisfies certain constraints defined by the given group structure.
     *
     * @param level The current recursion depth or slot that is being assigned.
     * @param position A sequence representing the current assignment of indices to positions.
     * @param sortedIndices A sequence of candidate indices sorted in a specific order.
     * @param used A sequence of flags indicating whether each index in `sortedIndices` has been used.
     * @param G The group structure defining the constraints for valid permutations.
     * @return True if a valid assignment that satisfies the constraints is found; otherwise, false.
     */
    inline bool assignslot(int level, seque<int> &position, const seque<int> &sortedIndices, seque<bool> &used,
                           const Group &G)
    {
        const int n = getsize(position);
        // If all slots are assigned, check if the permutation is valid in G
        if (level == n)
        {
            return isvalidfullpermutation(position, G);
        }

        // Try each candidate index (in sorted order) that has not been used yet.
        for (int idx: sortedIndices)
        {
            if (!used[idx])
            {
                position[level] = idx;
                if (canextend(position, level, G))
                {
                    used[idx] = true;
                    if (assignslot(level + 1, position, sortedIndices, used, G))
                    {
                        return true;
                    }
                    used[idx] = false;
                }
                // Reset the position slot if assignment failed
                position[level] = UNSET;
            }
        }
        return false;
    }
} // namespace hidden

/**
 * Computes the canonical form of a given sequence of labels under the constraints of a group of permutations.
 *
 * If the group is empty or the permutation sizes do not match the size of the label sequence,
 * the original label sequence is returned as is. Otherwise, the function determines a canonical
 * permutation of the labels using the group.
 *
 * @param labels The input sequence of labels for which the canonical form is to be computed.
 * @param G The group of permutations that defines the equivalence under which the canonical form is derived.
 * @return A sequence representing the canonical form of the given labels under the provided group of permutations.
 */
inline seque<int> getcanonicalform(const seque<int> &labels, const hidden::Group &G)
{
    using namespace hidden;
    const int n = getsize(labels);

    // If the group is empty or the permutation sizes do not match, return the original labels.
    if (getsize(G) == 0 || getsize(G[0]) != n)
    {
        return labels;
    }

    // (1) Build sortedIndices sorted by corresponding label values in ascending order.
    seque<int> sortedIndices(n);
    for (int i = 0; i < n; ++i)
    {
        sortedIndices[i] = i;
    }
    std::sort(sortedIndices.begin(), sortedIndices.end(),
              [&labels](int a, int b) { return labels[a] < labels[b]; });

    // (2) Initialize position with UNSET and a boolean array for used indices.
    seque<int> position(n, UNSET);
    seque<bool> used(n, false);

    // (3) Recursively assign slots to obtain a canonical permutation.
    if (!assignslot(0, position, sortedIndices, used, G))
    {
        throw std::runtime_error("No valid labeling found for the group.");
    }

    // Build the final canonical labeling by mapping indices to original labels.
    seque<int> canonical(n);
    for (int k = 0; k < n; ++k)
    {
        canonical[k] = labels[position[k]];
    }
    return canonical;
}

#endif // SYMMETRIES_HPP
