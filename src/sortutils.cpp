#include "sortutils.hpp"

#include <algorithm>
#include <array>
#include <chrono>
#include <execution>
#include <iostream>
#include <string>

void initializeRawArrayFromStdArray(const SourceArray& source, int dest[])
{
    for (size_t i = 0; i < source.size(); ++i)
    {
        dest[i] = source[i];
    }
}

void organPipeStdArray(SourceArray& data)
{
    const std::size_t n = data.size();
    const std::size_t half = n / 2;
    SourceArray organPipeArray = {};
    for (std::size_t i = 0; i < half; ++i)
    {
        organPipeArray[i] = data[i];
    }
    if (n % 2 != 0)
    {
        organPipeArray[half] = data[half];
    }
    for (std::size_t i = 0; i < half; ++i)
    {
        organPipeArray[n - 1 - i] = organPipeArray[i];
    }
    data = organPipeArray;
}

template <typename Container, typename SortFunction>
void measureSortTime(const std::string& label, Container data, SortFunction sortFunction)
{
    std::chrono::milliseconds totalDuration = {};
    for (std::uint8_t i = 0; i < HOW_MANY_TIMES; i++)
    {
        auto tempData = data;
        const auto start = std::chrono::steady_clock::now();
        sortFunction(tempData);
        const auto end = std::chrono::steady_clock::now();
        const auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end - start);
        totalDuration += std::chrono::duration_cast<std::chrono::milliseconds>(duration);
    }

    std::cout << "        " << std::left << std::setw(15) << label + " Time" << " : "
              << std::setw(3) << (totalDuration.count() / HOW_MANY_TIMES) << " ms\n";
}

void evaluateRawArray(const SourceArray& random, const SourceArray& sorted, const SourceArray& reversed, const SourceArray& organPipe, const SourceArray& rotated)
{
    std::cout << " --- Raw Array Performance ---\n\n";

    int randomArray[HOW_MANY_ELEMENTS];
    int sortedArray[HOW_MANY_ELEMENTS];
    int reversedArray[HOW_MANY_ELEMENTS];
    int organPipeArray[HOW_MANY_ELEMENTS];
    int rotatedArray[HOW_MANY_ELEMENTS];

    initializeRawArrayFromStdArray(random, randomArray);
    initializeRawArrayFromStdArray(sorted, sortedArray);
    initializeRawArrayFromStdArray(reversed, reversedArray);
    initializeRawArrayFromStdArray(organPipe, organPipeArray);
    initializeRawArrayFromStdArray(rotated, rotatedArray);

    const char* labels[] = { "Random", "Sorted", "Reversed", "Organ Pipe", "Rotated" };
    int* rawArrays[] = { randomArray, sortedArray, reversedArray, organPipeArray, rotatedArray };

    std::cout << "Sequential\n";
    for (size_t i = 0; i < 5; ++i)
    {
        measureSortTime(
            labels[i],
            rawArrays[i],
            [](int* data)
            {
                std::ranges::sort(data, data + HOW_MANY_ELEMENTS);
            });
    }

    std::cout << "\nParallel\n";
    for (size_t i = 0; i < 5; ++i)
    {
        measureSortTime(
            labels[i],
            rawArrays[i],
            [](int* data)
            {
                std::sort(std::execution::par, data, data + HOW_MANY_ELEMENTS);
            });
    }
}

void evaluateStdArray(const SourceArray& random, const SourceArray& sorted, const SourceArray& reversed, const SourceArray& organPipe, const SourceArray& rotated)
{
    std::cout << " --- std::array Performance ---\n\n";

    const SourceArray arrays[] = { random, sorted, reversed, organPipe, rotated };
    const std::string labels[] = { "Random", "Sorted", "Reversed", "Organ Pipe", "Rotated" };

    std::cout << "Sequential\n";
    for (size_t i = 0; i < 5; ++i)
    {
        measureSortTime(
            labels[i],
            arrays[i],
            [](SourceArray& data)
            {
                std::ranges::sort(data);
            });
    }

    std::cout << "\nParallel\n";
    for (size_t i = 0; i < 5; ++i)
    {
        measureSortTime(
            labels[i],
            arrays[i],
            [](SourceArray& data)
            {
                std::sort(std::execution::par, data.begin(), data.end());
            });
    }
}

void evaluateStdVector(const SourceArray& random, const SourceArray& sorted, const SourceArray& reversed, const SourceArray& organPipe, const SourceArray& rotated)
{
    std::cout << " --- std::vector Performance ---\n\n";

    const std::vector vectors = {
        std::vector(random.begin(), random.end()),
        std::vector(sorted.begin(), sorted.end()),
        std::vector(reversed.begin(), reversed.end()),
        std::vector(organPipe.begin(), organPipe.end()),
        std::vector(rotated.begin(), rotated.end())
    };

    const std::string labels[] = { "Random", "Sorted", "Reversed", "Organ Pipe", "Rotated" };

    std::cout << "Sequential\n";
    for (size_t i = 0; i < vectors.size(); ++i)
    {
        measureSortTime(
            labels[i],
            vectors[i],
            [](std::vector<int>& data)
            {
                std::ranges::sort(data);
            });
    }

    std::cout << "\nParallel\n";
    for (size_t i = 0; i < vectors.size(); ++i)
    {
        measureSortTime(
            labels[i],
            vectors[i],
            [](std::vector<int>& data)
            {
                std::sort(std::execution::par, data.begin(), data.end());
            });
    }
}
