#include <cstdint>
#include <format>
#include <iostream>
#include <random>
#include <vector>

class DistributionPair
{
public:
    std::uint32_t minValue;
    std::uint32_t maxValue;
    std::uint32_t count;

    DistributionPair(const std::uint32_t minValue, const std::uint32_t maxValue) :
        minValue(minValue), maxValue(maxValue), count(0)
    {
    }
};


std::vector<DistributionPair> generateUniformDistribution(const std::uint32_t howMany, const std::uint32_t min,
                                                          const std::uint32_t max, const std::uint8_t numberBins)
{
    std::random_device rd;
    std::default_random_engine engine(rd());
    std::uniform_int_distribution<std::uint32_t> distribution(min, max);

    std::vector<DistributionPair> data;
    const std::uint32_t binWidth = (max - min + 1) / numberBins;

    for (std::uint8_t i = 0; i < numberBins; ++i)
    {
        std::uint32_t binMin = min + i * binWidth;
        std::uint32_t binMax = (i == numberBins - 1) ? max : binMin + binWidth - 1;
        data.emplace_back(binMin, binMax);
    }

    for (std::uint32_t i = 0; i < howMany; ++i)
    {
        const std::uint32_t value = distribution(engine);
        for (auto &dp : data)
        {
            if (value >= dp.minValue && value <= dp.maxValue)
            {
                ++dp.count;
                break;
            }
        }
    }

    return data;
}


std::vector<DistributionPair> generateNormalDistribution(const std::uint32_t howMany, const float mean,
                                                         const float stdev, const std::uint8_t numberBins)
{
    std::random_device rd;
    std::default_random_engine engine(rd());
    std::normal_distribution<double> distribution(mean, stdev);

    std::vector<DistributionPair> data;
    const auto min = static_cast<std::uint32_t>(mean - 4 * stdev);
    const auto max = static_cast<std::uint32_t>(mean + 4 * stdev);
    const std::uint32_t binWidth = (max - min + 1) / numberBins;

    for (std::uint8_t i = 0; i < numberBins; ++i)
    {
        std::uint32_t binMin = min + i * binWidth;
        std::uint32_t binMax = (i == numberBins - 1) ? max : binMin + binWidth - 1;
        data.emplace_back(binMin, binMax);
    }

    for (std::uint32_t i = 0; i < howMany; ++i)
    {
        const auto value = static_cast<std::uint32_t>(distribution(engine));
        for (auto &dp : data)
        {
            if (value >= dp.minValue && value <= dp.maxValue)
            {
                ++dp.count;
                break;
            }
        }
    }

    return data;
}


std::vector<DistributionPair> generatePoissonDistribution(const std::uint32_t howMany, const std::uint8_t howOften,
                                                          const std::uint8_t numberBins)
{
    std::random_device rd;
    std::default_random_engine engine(rd());
    std::poisson_distribution<std::uint32_t> distribution(howOften);

    std::vector<DistributionPair> data;
    constexpr std::uint32_t min = 0;
    const std::uint32_t max = numberBins - 1;
    const std::uint32_t binWidth = (max - min + 1) / numberBins;

    for (std::uint8_t i = 0; i < numberBins; ++i)
    {
        std::uint32_t binMin = min + i * binWidth;
        std::uint32_t binMax = (i == numberBins - 1) ? max : binMin + binWidth - 1;
        data.emplace_back(binMin, binMax);
    }

    for (std::uint32_t i = 0; i < howMany; ++i)
    {
        const std::uint32_t value = distribution(engine);
        for (auto &dp : data)
        {
            if (value >= dp.minValue && value <= dp.maxValue)
            {
                ++dp.count;
                break;
            }
        }
    }

    return data;
}


void plotDistribution(const std::string &title, const std::vector<DistributionPair> &distribution,
                      const std::uint8_t maxPlotLineSize)
{
    std::cout << title << std::endl;

    std::uint32_t maxCount = 0;
    for (const auto &dp : distribution)
    {
        if (dp.count > maxCount)
        {
            maxCount = dp.count;
        }
    }

    for (const auto &dp : distribution)
    {
        std::cout << std::format("[{:>3},{:>4}] : ", dp.minValue, dp.maxValue);

        const std::uint32_t numDots = (maxCount > 0) ? (dp.count * maxPlotLineSize + maxCount - 1) / maxCount : 0;
        for (std::uint32_t i = 0; i < numDots; ++i)
        {
            std::cout << "*";
        }

        std::cout << std::endl;
    }
    std::cout << std::endl;
}


int main()
{
    const auto uniform = generateUniformDistribution(100000, 0, 79, 40);
    plotDistribution("--- Uniform ---", uniform, 80);

    const auto normal = generateNormalDistribution(100000, 50, 5, 40);
    plotDistribution("--- Normal ---", normal, 80);

    const auto poisson = generatePoissonDistribution(100000, 6, 40);
    plotDistribution("--- Poisson ---", poisson, 80);
}
