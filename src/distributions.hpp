#pragma once

#include <cstdint>
#include <string>
#include <vector>

/**
 * Represents a pair of minimum and maximum values for a distribution bin, along with the count of values in that bin.
 */
class DistributionPair
{
public:
    std::uint32_t minValue; ///< The minimum value of the bin.
    std::uint32_t maxValue; ///< The maximum value of the bin.
    std::uint32_t count; ///< The count of values falling within this bin.

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
 * Generates a uniform distribution of values and counts the occurrences in each bin.
 *
 * @param howMany The number of values to generate.
 * @param min The minimum value of the range.
 * @param max The maximum value of the range.
 * @param numberBins The number of bins to divide the range into.
 *
 * @return A vector of DistributionPair objects representing the distribution of values in the bins.
 */
std::vector<DistributionPair> generateUniformDistribution(std::uint32_t howMany, std::uint32_t min,
                                                          std::uint32_t max, std::uint8_t numberBins);

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
std::vector<DistributionPair> generateNormalDistribution(std::uint32_t howMany, float mean,
                                                         float stdev, std::uint8_t numberBins);

/**
 * Generates a Poisson distribution of values and counts the occurrences in each bin.
 *
 * @param howMany The number of values to generate.
 * @param howOften The average rate (lambda) of the Poisson distribution.
 * @param numberBins The number of bins to divide the range into.
 *
 * @return A vector of DistributionPair objects representing the distribution of values in the bins.
 */
std::vector<DistributionPair> generatePoissonDistribution(std::uint32_t howMany, std::uint8_t howOften,
                                                          std::uint8_t numberBins);

/**
 * Plots the distribution of values in the bins to the standard output.
 *
 * @param title The title to display before the plot.
 * @param distribution A vector of DistributionPair objects representing the distribution of values in the bins.
 * @param maxPlotLineSize The maximum number of plot characters per line.
 */
void plotDistribution(const std::string &title, const std::vector<DistributionPair> &distribution,
                      std::uint8_t maxPlotLineSize);