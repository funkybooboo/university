#include "sortutils.hpp"

#include <algorithm>
#include <array>
#include <chrono>
#include <execution>
#include <iostream>
#include <string>
#include <vector>

/**
 * Initializes a raw array from a standard array.
 *
 * @param source The source standard array to copy from.
 * @param dest The destination raw array where elements will be copied to.
 * @throws std::invalid_argument if the destination array is null.
 * @throws std::out_of_range if the size of the source array exceeds the capacity of the destination array.
 */
void initializeRawArrayFromStdArray(const SourceArray& source, int dest[])
{
    if (dest == nullptr)
    {
        throw std::invalid_argument("Destination array cannot be null.");
    }

    if (source.size() != HOW_MANY_ELEMENTS)
    {
        throw std::out_of_range("Source array size exceeds destination array capacity.");
    }

    std::ranges::copy(source, dest);
}

/**
 * Rearranges the elements of a standard array into an organ pipe structure.
 *
 * @param data The standard array to be rearranged.
 */
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

/**
 * Measures the time taken to sort a collection using the provided sorting function.
 *
 * @param label The label for the sorting operation, used in output.
 * @param data The collection to be sorted.
 * @param sortFunction The sorting function to use.
 */
template <typename Collection>
void measureSortTime(const std::string& label, Collection& data, auto& sortFunction)
{
    std::chrono::milliseconds totalDuration = {};
    for (std::uint8_t i = 0; i < HOW_MANY_TIMES; i++)
    {
        auto deepCopy = data;
        const auto start = std::chrono::steady_clock::now();
        sortFunction(deepCopy);
        const auto end = std::chrono::steady_clock::now();
        const auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end - start);
        totalDuration += std::chrono::duration_cast<std::chrono::milliseconds>(duration);
    }

    std::cout << "        " << std::left << std::setw(15) << label + " Time" << " : "
              << std::setw(3) << totalDuration.count() << " ms\n";
}

/**
 * Evaluates the performance of sorting various collections.
 *
 * @param randomArray A collection of randomly ordered elements.
 * @param sortedArray A collection of sorted elements.
 * @param reversedArray A collection of reversed elements.
 * @param organPipeArray A collection arranged in organ pipe order.
 * @param rotatedArray A collection that has been rotated.
 * @param type The type of the collection (e.g., "Raw Array", "std::array").
 * @param setupAndSortFunction A function to set up and sort the collection.
 * @param sequentialSortFunction The sorting function for sequential sorting.
 * @param parallelSortFunction The sorting function for parallel sorting.
 */
void evaluateCollection(const SourceArray& randomArray, const SourceArray& sortedArray, const SourceArray& reversedArray,
                        const SourceArray& organPipeArray, const SourceArray& rotatedArray, const std::string& type,
                        auto setupAndSortFunction, auto sequentialSortFunction, auto parallelSortFunction)
{
    std::cout << " --- " + type + " Performance ---\n\n";

    const char* labels[] = { "Random", "Sorted", "Reversed", "Organ Pipe", "Rotated" };
    const SourceArray* arrays[] = { &randomArray, &sortedArray, &reversedArray, &organPipeArray, &rotatedArray };

    auto evaluateSorting = [&](auto sortFunc)
    {
        for (size_t i = 0; i < 5; ++i)
        {
            setupAndSortFunction(*arrays[i], labels[i], sortFunc);
        }
    };

    std::cout << "Sequential\n";
    evaluateSorting(sequentialSortFunction);

    std::cout << "\nParallel\n";
    evaluateSorting(parallelSortFunction);

    std::cout << std::endl;
}

/**
 * Evaluates the performance of sorting raw arrays.
 *
 * @param random The randomly ordered raw array.
 * @param sorted The sorted raw array.
 * @param reversed The reversed raw array.
 * @param organPipe The organ pipe ordered raw array.
 * @param rotated The rotated raw array.
 */
void evaluateRawArray(const SourceArray& random, const SourceArray& sorted, const SourceArray& reversed,
                      const SourceArray& organPipe, const SourceArray& rotated)
{
    auto setupAndSortFunction = [](const SourceArray& source, const char* label, auto& sortFunction)
    {
        auto rawArray = new int[HOW_MANY_ELEMENTS];
        initializeRawArrayFromStdArray(source, rawArray);
        measureSortTime(label, rawArray, sortFunction);
        delete[] rawArray;
    };

    auto sequentialSortFunction = [](int* data)
    {
        std::ranges::sort(data, data + HOW_MANY_ELEMENTS);
    };

    auto parallelSortFunction = [](int* data)
    {
        std::sort(std::execution::par, data, data + HOW_MANY_ELEMENTS);
    };

    evaluateCollection(random, sorted, reversed, organPipe, rotated, "Raw Array", setupAndSortFunction, sequentialSortFunction, parallelSortFunction);
}

/**
 * Evaluates the performance of sorting standard arrays.
 *
 * @param random The randomly ordered standard array.
 * @param sorted The sorted standard array.
 * @param reversed The reversed standard array.
 * @param organPipe The organ pipe ordered standard array.
 * @param rotated The rotated standard array.
 */
void evaluateStdArray(const SourceArray& random, const SourceArray& sorted, const SourceArray& reversed,
                      const SourceArray& organPipe, const SourceArray& rotated)
{
    auto setupAndSortFunction = [](const SourceArray& source, const std::string& label, auto& sortFunction)
    {
        measureSortTime(label, source, sortFunction);
    };

    auto sequentialSortFunction = [](SourceArray& data)
    {
        std::ranges::sort(data);
    };

    auto parallelSortFunction = [](SourceArray& data)
    {
        std::sort(std::execution::par, data.begin(), data.end());
    };

    evaluateCollection(random, sorted, reversed, organPipe, rotated, "std::array", setupAndSortFunction, sequentialSortFunction, parallelSortFunction);
}

/**
 * Evaluates the performance of sorting standard vectors.
 *
 * @param random The randomly ordered standard vector.
 * @param sorted The sorted standard vector.
 * @param reversed The reversed standard vector.
 * @param organPipe The organ pipe ordered standard vector.
 * @param rotated The rotated standard vector.
 */
void evaluateStdVector(const SourceArray& random, const SourceArray& sorted, const SourceArray& reversed,
                       const SourceArray& organPipe, const SourceArray& rotated)
{
    auto setupAndSortFunction = [](const SourceArray& source, const std::string& label, auto& sortFunction)
    {
        std::vector dataCopy(source.begin(), source.end());
        measureSortTime(label, dataCopy, sortFunction);
    };

    auto sequentialSortFunction = [](std::vector<int>& data)
    {
        std::ranges::sort(data);
    };

    auto parallelSortFunction = [](std::vector<int>& data)
    {
        std::sort(std::execution::par, data.begin(), data.end());
    };

    evaluateCollection(random, sorted, reversed, organPipe, rotated, "std::vector", setupAndSortFunction, sequentialSortFunction, parallelSortFunction);
}
