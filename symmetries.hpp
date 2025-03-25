#ifndef SYMMETRIES_HPP
#define SYMMETRIES_HPP
#include <algorithm>
#include <iostream>
#include <limits>
#include <stdexcept>
#include <vector>
#include "sek.hpp"

namespace hidden
{
    using Permutation = sek<size_t>;
    using Group = sek<Permutation>;
    static const size_t UNSET = (std::numeric_limits<size_t>::max)();

    bool isvalidfullpermutation(const sek<size_t> &position, const Group &G)
    {
        for (const auto &g: G)
        {
            // Quick check on size match:
            if (getsize(g) != getsize(position))
                continue;
            bool match = true;
            for (size_t i = 0; i < getsize(position); i++)
            {
                if (g[i] != position[i])
                {
                    match = false;
                    break;
                }
            }
            if (match)
            {
                return true; // Found a match
            }
        }
        return false;
    }

    bool canextend(const sek<size_t> &position, size_t upTo, const Group &G)
    {
        for (const auto &g: G)
        {
            if (g.size() != position.size())
                continue;

            bool match = true;
            for (size_t k = 0; k <= upTo; k++)
            {
                if (position[k] != UNSET && position[k] != g[k])
                {
                    match = false;
                    break;
                }
            }
            if (match)
            {
                return true; // at least one permutation is consistent so far
            }
        }
        return false;
    }

    bool assignslot(size_t level, sek<size_t> &position, const sek<size_t> &sortedIndices, sek<bool> &used,
                    const Group &G)
    {
        size_t n = getsize(position);

        // If all slots assigned, check membership
        if (level == n)
        {
            return isvalidfullpermutation(position, G);
        }

        for (size_t idx: sortedIndices)
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
                position[level] = UNSET;
            }
        }
        return false;
    }
} // namespace detail
sek<size_t> getcanonicalform(const sek<size_t> &labels, const Group &G)
{
    using detail;
    size_t n = getsize(labels);
    // Basic check
    if (G.empty() || G[0].size() != n)
    {
        throw std::runtime_error("Group dimension does not match label size.");
    }

    // (1) Build sortedIndices by ascending label
    std::vector<size_t> sortedIndices(n);
    for (size_t i = 0; i < n; i++)
    {
        sortedIndices[i] = i;
    }
    std::sort(sortedIndices.begin(), sortedIndices.end(), [&](size_t a, size_t b) { return labels[a] < labels[b]; });

    // (2) Initialize position[] = UNSET, used[] = false
    std::vector<size_t> position(n, UNSET);
    std::vector<bool> used(n, false);

    // (3) Recursively assign
    bool success = assignSlot(0, position, sortedIndices, used, G);

    if (!success)
    {
        throw std::runtime_error("No valid labeling found in the group.");
    }

    // Build final canonical labeling
    std::vector<size_t> canonical(n);
    for (size_t k = 0; k < n; k++)
    {
        canonical[k] = labels[position[k]];
    }
    return canonical;
}

#endif // SYMMETRIES_HPP
