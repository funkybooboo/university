#include "distributions.hpp"

#include <cstdint>
#include <format>
#include <iostream>
#include <random>
#include <vector>

/**
 * Initializes bins for a distribution based on the minimum value, maximum value, and number of bins.
 *
 * @param min The minimum value of the range.
 * @param max The maximum value of the range.
 * @param numberBins The number of bins to be created.
 *
 * @return A vector of DistributionPair objects representing the bins.
 */
std::vector<DistributionPair> initializeBins(const std::uint32_t min, const std::uint32_t max,
                                             const std::uint8_t numberBins)
{
    std::vector<DistributionPair> bins;
    // Calculate the width of each bin
    const std::uint32_t binWidth = (max - min + 1) / numberBins;

    for (std::uint8_t i = 0; i < numberBins; ++i)
    {
        // Compute the min and max values for each bin
        std::uint32_t binMin = min + i * binWidth;
        std::uint32_t binMax = (i == numberBins - 1) ? max : binMin + binWidth - 1;
        bins.emplace_back(binMin, binMax); // Create a new bin and add it to the vector
    }

    return bins;
}

/**
 * Counts the number of values that fall into each bin based on the given distribution.
 *
 * @tparam Distribution The type of distribution to be used for generating values.
 *
 * @param bins A vector of DistributionPair objects representing the bins.
 * @param distribution The distribution object used to generate values.
 * @param engine The random number engine used by the distribution.
 * @param howMany The number of values to generate and count.
 */
template <typename Distribution>
void countValues(std::vector<DistributionPair> &bins, Distribution &distribution, std::default_random_engine &engine,
                 const std::uint32_t howMany)
{
    for (std::uint32_t i = 0; i < howMany; ++i)
    {
        const auto value = static_cast<std::uint32_t>(distribution(engine)); // Generate a value
        if (value <= bins[0].minValue)
        {
            ++bins[0].count;
            continue;
        }
        if (value >= bins[bins.size() - 1].maxValue)
        {
            ++bins[bins.size() - 1].count;
            continue;
        }
        for (auto &dp : bins)
        {
            // Increment the count for the bin that contains the value
            if (value >= dp.minValue && value <= dp.maxValue)
            {
                ++dp.count;
                break;
            }
        }
    }
}

std::vector<DistributionPair> generateUniformDistribution(const std::uint32_t howMany, const std::uint32_t min,
                                                          const std::uint32_t max, const std::uint8_t numberBins)
{
    std::random_device rd; // Obtain a random number from hardware
    std::default_random_engine engine(rd()); // Seed the engine
    std::uniform_int_distribution distribution(min, max); // Define the uniform distribution

    auto bins = initializeBins(min, max, numberBins); // Initialize bins
    countValues(bins, distribution, engine, howMany); // Count values in bins

    return bins;
}

std::vector<DistributionPair> generateNormalDistribution(const std::uint32_t howMany, const float mean,
                                                         const float stdev, const std::uint8_t numberBins)
{
    std::random_device rd; // Obtain a random number from hardware
    std::default_random_engine engine(rd()); // Seed the engine
    std::normal_distribution distribution(mean, stdev); // Define the normal distribution

    // Define the range for bins based on mean and standard deviation
    const auto min = static_cast<std::uint32_t>(std::floor(mean - 4 * stdev));
    const auto max = static_cast<std::uint32_t>(std::ceil(mean + 4 * stdev - 1));
    auto bins = initializeBins(min, max, numberBins); // Initialize bins
    countValues(bins, distribution, engine, howMany); // Count values in bins

    return bins;
}

std::vector<DistributionPair> generatePoissonDistribution(const std::uint32_t howMany, const std::uint8_t howOften,
                                                          const std::uint8_t numberBins)
{
    std::random_device rd; // Obtain a random number from hardware
    std::default_random_engine engine(rd()); // Seed the engine
    std::poisson_distribution distribution(howOften); // Define the Poisson distribution

    // Define the range for bins based on the number of bins
    constexpr std::uint32_t min = 0;
    const auto max = static_cast<std::uint32_t>(howOften * 3 - 1);
    auto bins = initializeBins(min, max, numberBins); // Initialize bins
    countValues(bins, distribution, engine, howMany); // Count values in bins

    return bins;
}

void plotDistribution(const std::string &title, const std::vector<DistributionPair> &distribution,
                      const std::uint8_t maxPlotLineSize)
{
    std::cout << title << std::endl;

    // Find the maximum count for scaling the plot
    std::uint32_t maxCount = 0;
    for (const auto &dp : distribution)
    {
        if (dp.count > maxCount)
        {
            maxCount = dp.count;
        }
    }

    // Print each bin's distribution
    for (const auto &dp : distribution)
    {
        std::cout << std::format("[{:>3},{:>4}] : ", dp.minValue, dp.maxValue);

        // Calculate the number of plot characters based on the bin count
        const std::uint32_t numDots = (maxCount > 0) ? (dp.count * maxPlotLineSize + maxCount - 1) / maxCount : 0;

        std::cout << std::format("{}", std::string(numDots, '*')) << std::endl;
    }
    std::cout << std::endl;
}
