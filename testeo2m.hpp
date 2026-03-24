#include "m2m.hpp"
#include <array>
#include <chrono>
#include <cstddef>
#include <random>

std::array<int, 8> generateRandomArray(int nmax)
{
    std::array<int, 8> arr;

    // Create a random number generator seeded with a random_device.
    std::random_device rd;
    std::mt19937 gen(rd());

    // Ensure nmax is large enough to allow unique values.
    if (nmax < arr.size())
    {
        throw std::invalid_argument(
            "nmax must be at least 8 to generate unique numbers.");
    }

    // Create a vector containing all possible values and shuffle it.
    std::vector<int> values(nmax);
    std::iota(values.begin(), values.end(),
              0); // Fill values with 0, 1, ..., nmax-1
    std::shuffle(values.begin(), values.end(), gen);
    // Copy the first 8 unique values to the array.
    std::copy_n(values.begin(), arr.size(), arr.begin());
    return arr;
}

inline void testeo2m()
{
    int nel, nmax;
    seque<int> els;
    m2m mm;
    o2m om;
    {
        constexpr int ntype = 6;
        switch (ntype)
        {
            case 6:
                nel = 400;
                nmax = nel * nel * nel;
                std::cout << "Started" << std::endl;
                {
                    const int stride1 = nel + 1;
                    const int stride2 = stride1 * stride1;
                    for (int iex = 0; iex < nel; ++iex)
                        for (int iey = 0; iey < nel; ++iey)
                            for (int iez = 0; iez < nel; ++iez)
                            {
                                const int base = iex + iey * stride1 + iez * stride2;
                                seque<int> nodes(8);
                                nodes[0] = base;
                                nodes[1] = base + 1;
                                nodes[2] = base + 1 + stride1;
                                nodes[3] = base + stride1;
                                nodes[4] = base + stride2;
                                nodes[5] = base + 1 + stride2;
                                nodes[6] = base + 1 + stride1 + stride2;
                                nodes[7] = base + stride1 + stride2;
                                appendelement(om, nodes);
                            }
                }
                break;
            case 4:
                nel = 12000;
                nmax = nel * nel;
                setsize(els, nmax);
                std::cout << "Started" << std::endl;
                setsize(om, nmax);
                std::cout << "Started" << std::endl;
                for (int iex = 0; iex < nel; ++iex)
                    for (int iey = 0; iey < nel; ++iey)
                    {
                        seque<int> nodes(4);
                        nodes[0] = iex + iey * (nel + 1);
                        nodes[1] = (iex + 1) + iey * (nel + 1);
                        nodes[2] = (iex + 1) + (iey + 1) * (nel + 1);
                        nodes[3] = iex + (iey + 1) * (nel + 1);
                        appendelement(om, nodes);
                    }
                break;
            default:
                break;
        }

        mm.nfrome = om;
        o2m &om1 = mm.nfrome;
        m2m result;
        auto start_time = std::chrono::high_resolution_clock::now();
        std::cout << "tr beg\n";
        o2m om2 = Tr(om1);
        std::cout << "tr end\n";
        o2m om3;
        std::cout << "mult beg\n";
        om3 = om2 * om1;
        auto end_time = std::chrono::high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(
            end_time - start_time);
        std::cout << "Duration: " << duration.count() << " milliseconds"
                << std::endl;
        std::cout << "mult end\n";
        std::cout << "maxnode 3=" << om3.maxnode << std::endl;
        std::cout << "maxnode 2=" << om2.maxnode << std::endl;
        std::cout << result.nfrome[0] << std::endl;
        std::cout << result.nfrome.lnods[0] << std::endl;
    }
}
