#ifndef SYMMETRIES_HPP
#define SYMMETRIES_HPP

#include <algorithm>
#include <limits>
#include <stdexcept>
#include <vector>
#include "sek.hpp"

namespace hidden {

    using Permutation = sek<size_t>;
    using Group = sek<Permutation>;
    static const size_t UNSET = (std::numeric_limits<size_t>::max)();

    inline bool isvalidfullpermutation(const sek<size_t>& position, const Group& G)
    {
        const size_t posSize = getsize(position);
        for (const auto &perm : G)
        {
            if (getsize(perm) != posSize)
                continue;

            bool match = true;
            for (size_t i = 0; i < posSize; ++i)
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

    inline bool canextend(const sek<size_t>& position, size_t upTo, const Group& G)
    {
        for (const auto &perm : G)
        {
            if (getsize(perm) != getsize(position))
                continue;

            bool match = true;
            // Check consistency up to 'upTo' index
            for (size_t k = 0; k <= upTo; ++k)
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

    inline bool assignslot(size_t level, sek<size_t>& position, const sek<size_t>& sortedIndices, sek<bool>& used,
                            const Group& G)
    {
        const size_t n = getsize(position);
        // If all slots are assigned, check if the permutation is valid in G
        if (level == n)
        {
            return isvalidfullpermutation(position, G);
        }

        // Try each candidate index (in sorted order) that has not been used yet.
        for (size_t idx : sortedIndices)
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

inline sek<size_t> getcanonicalform(const sek<size_t>& labels, const hidden::Group& G)
{
    using namespace hidden;
    const size_t n = getsize(labels);

    // If the group is empty or the permutation sizes do not match, return the original labels.
    if (getsize(G) == 0 || getsize(G[0]) != n)
    {
        return labels;
    }

    // (1) Build sortedIndices sorted by corresponding label values in ascending order.
    sek<size_t> sortedIndices(n);
    for (size_t i = 0; i < n; ++i)
    {
        sortedIndices[i] = i;
    }
    std::sort(sortedIndices.begin(), sortedIndices.end(),
              [&labels](size_t a, size_t b) { return labels[a] < labels[b]; });

    // (2) Initialize position with UNSET and a boolean array for used indices.
    sek<size_t> position(n, UNSET);
    sek<bool> used(n, false);

    // (3) Recursively assign slots to obtain a canonical permutation.
    if (!assignslot(0, position, sortedIndices, used, G))
    {
        throw std::runtime_error("No valid labeling found for the group.");
    }

    // Build the final canonical labeling by mapping indices to original labels.
    sek<size_t> canonical(n);
    for (size_t k = 0; k < n; ++k)
    {
        canonical[k] = labels[position[k]];
    }
    return canonical;
}

#endif // SYMMETRIES_HPP
