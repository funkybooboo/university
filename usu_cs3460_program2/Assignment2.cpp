#include <cstdint>
#include <format>
#include <iostream>
#include <random>
#include <vector>

/**
 * Represents a pair of minimum and maximum values for a distribution bin, along with the count of values in that bin.
 */
class DistributionPair
{
  public:
    std::uint32_t minValue; ///< The minimum value of the bin.
    std::uint32_t maxValue; ///< The maximum value of the bin.
    std::uint32_t count;    ///< The count of values falling within this bin.

    /**
     * Constructs a DistributionPair with the specified minimum and maximum values.
     *
     * @param minValue The minimum value of the bin.
     * @param maxValue The maximum value of the bin.
     */
    DistributionPair(const std::uint32_t minValue, const std::uint32_t maxValue) :
        minValue(minValue), maxValue(maxValue), count(0)
    {
    }
};

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
void countValues(std::vector<DistributionPair>& bins, Distribution& distribution, std::default_random_engine& engine,
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
        for (auto& dp : bins)
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

/**
 * Generates a uniform distribution of values and counts the occurrences in each bin.
 *
 * @param howMany The number of values to generate.
 * @param min The minimum value of the range.
 * @param max The maximum value of the range.
 * @param numberBins The number of bins to divide the range into.
 *
 * @return A vector of DistributionPair objects representing the distribution of values in the bins.
 */
std::vector<DistributionPair> generateUniformDistribution(const std::uint32_t howMany, const std::uint32_t min,
                                                          const std::uint32_t max, const std::uint8_t numberBins)
{
    std::random_device rd;                                // Obtain a random number from hardware
    std::default_random_engine engine(rd());              // Seed the engine
    std::uniform_int_distribution distribution(min, max); // Define the uniform distribution

    auto bins = initializeBins(min, max, numberBins); // Initialize bins
    countValues(bins, distribution, engine, howMany); // Count values in bins

    return bins;
}

/**
 * Generates a normal distribution of values and counts the occurrences in each bin.
 *
 * @param howMany The number of values to generate.
 * @param mean The mean of the normal distribution.
 * @param stdev The standard deviation of the normal distribution.
 * @param numberBins The number of bins to divide the range into.
 *
 * @return A vector of DistributionPair objects representing the distribution of values in the bins.
 */
std::vector<DistributionPair> generateNormalDistribution(const std::uint32_t howMany, const float mean,
                                                         const float stdev, const std::uint8_t numberBins)
{
    std::random_device rd;                              // Obtain a random number from hardware
    std::default_random_engine engine(rd());            // Seed the engine
    std::normal_distribution distribution(mean, stdev); // Define the normal distribution

    // Define the range for bins based on mean and standard deviation
    const auto min = static_cast<std::uint32_t>(std::floor(mean - 4 * stdev));
    const auto max = static_cast<std::uint32_t>(std::ceil(mean + 4 * stdev - 1));
    auto bins = initializeBins(min, max, numberBins); // Initialize bins
    countValues(bins, distribution, engine, howMany); // Count values in bins

    return bins;
}

/**
 * Generates a Poisson distribution of values and counts the occurrences in each bin.
 *
 * @param howMany The number of values to generate.
 * @param howOften The average rate (lambda) of the Poisson distribution.
 * @param numberBins The number of bins to divide the range into.
 *
 * @return A vector of DistributionPair objects representing the distribution of values in the bins.
 */
std::vector<DistributionPair> generatePoissonDistribution(const std::uint32_t howMany, const std::uint8_t howOften,
                                                          const std::uint8_t numberBins)
{
    std::random_device rd;                            // Obtain a random number from hardware
    std::default_random_engine engine(rd());          // Seed the engine
    std::poisson_distribution distribution(howOften); // Define the Poisson distribution

    // Define the range for bins based on the number of bins
    constexpr std::uint32_t min = 0;
    const auto max = static_cast<std::uint32_t>(numberBins - 1);
    auto bins = initializeBins(min, max, numberBins); // Initialize bins
    countValues(bins, distribution, engine, howMany); // Count values in bins

    return bins;
}

/**
 * Plots the distribution of values in the bins to the standard output.
 *
 * @param title The title to display before the plot.
 * @param distribution A vector of DistributionPair objects representing the distribution of values in the bins.
 * @param maxPlotLineSize The maximum number of plot characters per line.
 */
void plotDistribution(const std::string& title, const std::vector<DistributionPair>& distribution,
                      const std::uint8_t maxPlotLineSize)
{
    std::cout << title << std::endl;

    // Find the maximum count for scaling the plot
    std::uint32_t maxCount = 0;
    for (const auto& dp : distribution)
    {
        if (dp.count > maxCount)
        {
            maxCount = dp.count;
        }
    }

    // Print each bin's distribution
    for (const auto& dp : distribution)
    {
        std::cout << std::format("[{:>3},{:>4}] : ", dp.minValue, dp.maxValue);

        // Calculate the number of plot characters based on the bin count
        const std::uint32_t numDots = (maxCount > 0) ? (dp.count * maxPlotLineSize + maxCount - 1) / maxCount : 0;
        for (std::uint32_t i = 0; i < numDots; ++i)
        {
            std::cout << "*";
        }

        std::cout << std::endl;
    }
    std::cout << std::endl;
}

void test();

int main()
{
    // Generate and plot uniform distribution
    const auto uniform = generateUniformDistribution(100000, 0, 79, 40);
    plotDistribution("--- Uniform ---", uniform, 80);

    // Generate and plot normal distribution
    const auto normal = generateNormalDistribution(100000, 50, 5, 40);
    plotDistribution("--- Normal ---", normal, 80);

    // Generate and plot Poisson distribution
    const auto poisson = generatePoissonDistribution(100000, 6, 40);
    plotDistribution("--- Poisson ---", poisson, 80);

    test();
}

// ------------------------------------------------------------------
//
// Testing Code
//
// ------------------------------------------------------------------
#include <functional>
#include <numeric>
#include <string>

namespace testing::detail
{
    using namespace std::string_literals;

    using Bins = std::vector<std::pair<std::uint32_t, std::uint32_t>>;
    using DistFunc = std::function<std::vector<DistributionPair>()>;

#define CS3460_ASSERT_EQ(expected, actual, message)                    \
    if (expected != actual)                                            \
    {                                                                  \
        fail(message, "[ Expected", expected, "but got", actual, "]"); \
        return;                                                        \
    }

#define CS3460_CASE(x) \
    [] {               \
        return x;      \
    };                 \
    std::cout << " Case " << #x << "\n";

    template <typename Message>
    void failInternal(const Message& message)
    {
        std::cout << message << " ";
    }

    template <typename Message, typename... Messages>
    void failInternal(const Message& message, const Messages&... messages)
    {
        failInternal(message);
        failInternal(messages...);
    }

    template <typename... Messages>
    void fail(const Messages&... messages)
    {
        std::cout << "  Assertion failed: ";
        failInternal(messages...);
        std::cout << "\n";
    }

    Bins generateBins(const std::uint32_t min, const std::uint32_t max, const std::uint8_t numberBins)
    {
        const auto binRange = (max - min) / numberBins;
        auto minBin = min;
        auto maxBin = min + binRange;

        Bins results(numberBins);
        for (std::uint8_t bin = 0u; bin < numberBins; bin++)
        {
            results[bin] = { minBin, maxBin };
            minBin = maxBin + 1;
            maxBin = minBin + binRange;
        }

        return results;
    }

    void returnsTheExpectedBins(const DistFunc& func, const Bins& bins)
    {
        const auto result = func();
        CS3460_ASSERT_EQ(bins.size(), result.size(), "Wrong number of bins");
        for (auto i = 0u; i < bins.size(); i++)
        {
            CS3460_ASSERT_EQ(bins[i].first, result[i].minValue, "Wrong minimum value for bin "s + std::to_string(i));
            CS3460_ASSERT_EQ(bins[i].second, result[i].maxValue, "Wrong maximum value for bin "s + std::to_string(i));
        }
    }

    void hasTheCorrectTotalAcrossAllBins(const DistFunc& func, const std::uint32_t howMany)
    {
        const auto result = func();
        const auto add_counts = [](std::uint32_t total, const DistributionPair& bin)
        {
            return total + bin.count;
        };
        CS3460_ASSERT_EQ(howMany, std::accumulate(result.cbegin(), result.cend(), 0u, add_counts),
                         "Wrong number of elements across all bins");
    }

    void testUniformDistribution()
    {
        std::cout << "Testing generateUniformDistribution\n";
        auto func = CS3460_CASE(generateUniformDistribution(100000, 0, 79, 40));
        returnsTheExpectedBins(func, generateBins(0, 79, 40));
        hasTheCorrectTotalAcrossAllBins(func, 100000);

        auto func2 = CS3460_CASE(generateUniformDistribution(0, 50, 59, 10));
        returnsTheExpectedBins(func2, generateBins(50, 59, 10));

        auto func3 = CS3460_CASE(generateUniformDistribution(0, 0, 79, 40));
        hasTheCorrectTotalAcrossAllBins(func3, 0);

        auto func4 = CS3460_CASE(generateUniformDistribution(100000, 0, 79, 1));
        hasTheCorrectTotalAcrossAllBins(func4, 100000);
    }

    void testNormalDistribution()
    {
        std::cout << "Testing generateNormalDistribution\n";
        auto func = CS3460_CASE(generateNormalDistribution(100000, 50, 5, 40));
        returnsTheExpectedBins(func, generateBins(30, 69, 40));
        hasTheCorrectTotalAcrossAllBins(func, 100000);

        auto func2 = CS3460_CASE(generateNormalDistribution(0, 50, 5, 40));
        hasTheCorrectTotalAcrossAllBins(func2, 0);

        auto func3 = CS3460_CASE(generateNormalDistribution(100000, 20.5, 1.125, 9));
        returnsTheExpectedBins(func3, generateBins(16, 24, 9));
        hasTheCorrectTotalAcrossAllBins(func3, 100000);
    }

    void testPoissonDistribution()
    {
        std::cout << "Testing generatePoissonDistribution\n";
        auto func = CS3460_CASE(generatePoissonDistribution(100000, 6, 40));
        returnsTheExpectedBins(func, generateBins(0, 39, 40));
        hasTheCorrectTotalAcrossAllBins(func, 100000);

        auto func2 = CS3460_CASE(generatePoissonDistribution(0, 6, 40));
        hasTheCorrectTotalAcrossAllBins(func2, 0);

        auto func3 = CS3460_CASE(generatePoissonDistribution(100000, 255, 40));
        hasTheCorrectTotalAcrossAllBins(func3, 100000);
    }
} // namespace testing::detail

void test()
{
    using namespace testing::detail;

    testUniformDistribution();
    testNormalDistribution();
    testPoissonDistribution();

    std::cout << "\n\n";
}
