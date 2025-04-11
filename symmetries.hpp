#ifndef SYMMETRIES_HPP
#define SYMMETRIES_HPP

#include <algorithm>
#include <limits>
#include <stdexcept>
#include <vector>
#include "seque.hpp"

namespace hidden
{
    using Permutation = seque<int>;
    using Group = seque<Permutation>;
    static const int UNSET = (std::numeric_limits<int>::max)();

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
