#include "sortutils.hpp"

#include <algorithm>
#include <array>
#include <chrono>
#include <execution>
#include <iostream>
#include <string>
#include <vector>

void initializeRawArrayFromStdArray(const SourceArray& source, int dest[])
{
    if (!dest)
    {
        throw std::invalid_argument("Destination array cannot be null.");
    }

    // Ensure the destination can hold the data (assuming you know the size of dest)
    if (source.size() > HOW_MANY_ELEMENTS)
    {
        throw std::out_of_range("Source array size exceeds destination array capacity.");
    }

    std::ranges::copy(source, dest);
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

    const char* labels[] = { "Random", "Sorted", "Reversed", "Organ Pipe", "Rotated" };
    const SourceArray* arrays[] = { &random, &sorted, &reversed, &organPipe, &rotated };

    std::cout << "Sequential\n";
    for (size_t i = 0; i < 5; ++i)
    {
        // Allocate a single raw array dynamically
        const auto rawArray = new int[HOW_MANY_ELEMENTS];

        // Initialize the raw array from the current SourceArray
        initializeRawArrayFromStdArray(*arrays[i], rawArray);

        // Measure the sort time using sequential sort
        measureSortTime(
            labels[i],
            rawArray,
            [](int* data)
            {
                std::ranges::sort(data, data + HOW_MANY_ELEMENTS);
            });

        // Clean up the dynamically allocated array
        delete[] rawArray;
    }

    std::cout << "\nParallel\n";
    for (size_t i = 0; i < 5; ++i)
    {
        // Allocate a single raw array dynamically
        const auto rawArray = new int[HOW_MANY_ELEMENTS];

        // Initialize the raw array from the current SourceArray
        initializeRawArrayFromStdArray(*arrays[i], rawArray);

        // Measure the sort time using parallel sort
        measureSortTime(
            labels[i],
            rawArray,
            [](int* data)
            {
                std::sort(std::execution::par, data, data + HOW_MANY_ELEMENTS);
            });

        // Clean up the dynamically allocated array
        delete[] rawArray;
    }
    std::cout << std::endl;
}

void evaluateStdArray(const SourceArray& random, const SourceArray& sorted, const SourceArray& reversed, const SourceArray& organPipe, const SourceArray& rotated)
{
    std::cout << " --- std::array Performance ---\n\n";

    // Store the SourceArrays in a dynamic container
    const SourceArray* arrays[] = { &random, &sorted, &reversed, &organPipe, &rotated };
    const std::string labels[] = { "Random", "Sorted", "Reversed", "Organ Pipe", "Rotated" };

    std::cout << "Sequential\n";
    for (size_t i = 0; i < 5; ++i)
    {
        // Create a copy of the current SourceArray
        const SourceArray dataCopy = *arrays[i];

        measureSortTime(
            labels[i],
            dataCopy,
            [](SourceArray& data)
            {
                std::ranges::sort(data);
            });
    }

    std::cout << "\nParallel\n";
    for (size_t i = 0; i < 5; ++i)
    {
        // Create a copy of the current SourceArray
        const SourceArray dataCopy = *arrays[i];

        measureSortTime(
            labels[i],
            dataCopy,
            [](SourceArray& data)
            {
                std::sort(std::execution::par, data.begin(), data.end());
            });
    }
    std::cout << std::endl;
}

void evaluateStdVector(const SourceArray& random, const SourceArray& sorted, const SourceArray& reversed, const SourceArray& organPipe, const SourceArray& rotated)
{
    std::cout << " --- std::vector Performance ---\n\n";

    const SourceArray* arrays[] = { &random, &sorted, &reversed, &organPipe, &rotated };
    const std::string labels[] = { "Random", "Sorted", "Reversed", "Organ Pipe", "Rotated" };

    std::cout << "Sequential\n";
    for (size_t i = 0; i < 5; ++i)
    {
        const std::vector dataCopy(arrays[i]->begin(), arrays[i]->end());

        measureSortTime(
            labels[i],
            dataCopy,
            [](std::vector<int>& data)
            {
                std::ranges::sort(data);
            });
    }

    std::cout << "\nParallel\n";
    for (size_t i = 0; i < 5; ++i)
    {
        const std::vector dataCopy(arrays[i]->begin(), arrays[i]->end());

        measureSortTime(
            labels[i],
            dataCopy,
            [](std::vector<int>& data)
            {
                std::sort(std::execution::par, data.begin(), data.end());
            });
    }
    std::cout << std::endl;
}
