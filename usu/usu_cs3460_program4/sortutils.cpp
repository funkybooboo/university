#include "sortutils.hpp"

#include <algorithm>
#include <array>
#include <chrono>
#include <cstring>
#include <execution>
#include <functional>
#include <iostream>
#include <ranges>
#include <stdexcept>
#include <string>
#include <vector>

/**
 * Initializes a raw array from a standard array.
 *
 * @param source The source standard array to copy from.
 * @param dest The destination raw array where elements will be copied to.
 * @throws std::invalid_argument if the destination array is null.
 * @throws std::out_of_range if the size of the source array exceeds the capacity of the destination array.
 * @note HOW_MANY_ELEMENTS represents the expected number of elements to copy.
 */
void initializeRawArrayFromStdArray(const SourceArray& source, int dest[])
{
    if (dest == nullptr)
    {
        throw std::invalid_argument("Destination array cannot be null.");
    }

    for (std::size_t i = 0; i < HOW_MANY_ELEMENTS; ++i)
    {
        dest[i] = source[i];
    }
}

/**
 * Rearranges the elements of a standard array into an organ pipe structure.
 *
 * @param data The standard array to be rearranged.
 * @note An organ pipe structure arranges elements so that the
 *       largest and smallest elements are at the ends, with a symmetrical pattern.
 */
void organPipeStdArray(SourceArray& data)
{
    const std::size_t n = HOW_MANY_ELEMENTS; // Fixed size from the constant
    SourceArray organPipe;                   // Create an array of the same size

    // Copy the first half of the array
    const std::size_t half = n / 2;
    for (std::size_t i = 0; i < half; ++i)
    {
        organPipe[i] = data[i];
    }

    // Copy the first half in reverse order to the second half
    for (std::size_t i = 0; i < half; ++i)
    {
        organPipe[n - 1 - i] = organPipe[i];
    }

    // Update the original array with the organ pipe structure
    for (std::size_t i = 0; i < n; ++i)
    {
        data[i] = organPipe[i];
    }
}

/**
 * Measures the time taken to sort a collection using the provided sorting function.
 *
 * @param label The label for the sorting operation, used in output.
 * @param data The Collection to be sorted.
 * @param copy Function to copy the data.
 * @param sort The sorting function to use.
 * @param cleanup Function to clean up after sorting.
 * @note This function performs multiple trials to get the sorting time.
 */
template <typename Collection, typename CopyFunction, typename SortFunction, typename CleanupFunction = std::function<void(Collection&)>>
void measureSortTime(const std::string& label, Collection& data, CopyFunction copy, SortFunction sort, CleanupFunction cleanup = [](Collection&)
                                                                                                       {
                                                                                                       })
{
    std::chrono::microseconds totalDuration = {}; // Total duration for multiple sort operations

    for (std::uint8_t i = 0; i < HOW_MANY_TIMES; i++)
    {
        auto dataCopy = copy(data); // Create a copy of the data

        const auto start = std::chrono::steady_clock::now(); // Start timer
        try
        {
            sort(dataCopy); // Sort the copied data
        }
        catch (const std::exception& e)
        {
            std::cerr << "Error sorting " << label << ": " << e.what() << '\n';
        }
        const auto end = std::chrono::steady_clock::now();                                        // End timer
        const auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end - start); // Calculate duration
        totalDuration += duration;                                                                // Accumulate total duration

        cleanup(dataCopy); // Clean up the copied data
    }

    // Print out the sorting time
    std::cout << "        " << std::left << std::setw(15) << label + " Time" << " : "
              << std::setw(3) << std::chrono::duration_cast<std::chrono::milliseconds>(totalDuration).count() << " ms\n";
}

/**
 * Evaluates the performance of sorting various collections.
 *
 * @param random A collection of randomly ordered elements.
 * @param sorted A collection of sorted elements.
 * @param reversed A collection of reversed elements.
 * @param organPipe A collection arranged in organ pipe order.
 * @param rotated A collection that has been rotated.
 * @param type The type of the collection (e.g., "Raw Array", "std::array").
 * @param sequentialSort The sorting function for sequential sorting.
 * @param parallelSort The sorting function for parallel sorting.
 * @param prepareAndMeasureSortTime A function to set up and sort the collection.
 */
void evaluateCollection(const SourceArray& random, const SourceArray& sorted, const SourceArray& reversed,
                        const SourceArray& organPipe, const SourceArray& rotated, const std::string& type,
                        auto& sequentialSort, auto& parallelSort, auto& prepareAndMeasureSortTime)
{
    std::cout << " --- " + type + " Performance ---\n\n";

    // Labels for the different types of data orderings
    const char* labels[] = { "Random", "Sorted", "Reversed", "Organ Pipe", "Rotated" };
    const SourceArray* arrays[] = { &random, &sorted, &reversed, &organPipe, &rotated };

    auto evaluateSorting = [&](auto& sort)
    {
        for (size_t i = 0; i < 5; ++i)
        {
            try
            {
                prepareAndMeasureSortTime(*arrays[i], labels[i], sort);
            }
            catch (const std::exception& e)
            {
                std::cerr << "Error evaluating " << labels[i] << ": " << e.what() << '\n';
            }
        }
    };

    std::cout << "Sequential\n";
    evaluateSorting(sequentialSort); // Evaluate sequential sorting

    std::cout << "\nParallel\n";
    evaluateSorting(parallelSort); // Evaluate parallel sorting

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
    try
    {
        auto copy = [](const int* data)
        {
            const auto dataCopy = new int[HOW_MANY_ELEMENTS];
            std::memcpy(dataCopy, data, HOW_MANY_ELEMENTS * sizeof(int));
            return dataCopy;
        };

        // Sequential sorting function for raw arrays
        auto sequentialSort = [](int* data)
        {
            std::sort(std::execution::seq, data, data + HOW_MANY_ELEMENTS);
        };

        // Parallel sorting function for raw arrays
        auto parallelSort = [](int* data)
        {
            std::sort(std::execution::par, data, data + HOW_MANY_ELEMENTS);
        };

        auto cleanup = [](const int* data)
        {
            delete[] data; // Clean up allocated memory
            data = nullptr;
        };

        // Prepare and measure sorting time for raw arrays
        auto prepareAndMeasureSortTime = [copy, cleanup](const SourceArray& data, const char* label, auto& sort)
        {
            auto dataCopy = new int[HOW_MANY_ELEMENTS];            // Allocate raw array
            initializeRawArrayFromStdArray(data, dataCopy);        // Initialize from standard array
            measureSortTime(label, dataCopy, copy, sort, cleanup); // Measure sorting time
            cleanup(dataCopy);                                     // Clean up
        };

        // Evaluate the performance of raw arrays
        evaluateCollection(random, sorted, reversed, organPipe, rotated, "Raw Array", sequentialSort, parallelSort, prepareAndMeasureSortTime);
    }
    catch (const std::exception& e)
    {
        std::cerr << "Error evaluating raw array performance: " << e.what() << '\n';
    }
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
    try
    {
        auto copy = [](const SourceArray& data)
        {
            return data; // Return copy of the array
        };

        // Sequential sorting function for standard arrays
        auto sequentialSort = [](SourceArray& data)
        {
            std::sort(std::execution::seq, data.begin(), data.end());
        };

        // Parallel sorting function for standard arrays
        auto parallelSort = [](SourceArray& data)
        {
            std::sort(std::execution::par, data.begin(), data.end());
        };

        // Prepare and measure sorting time for standard arrays
        auto prepareAndMeasureSortTime = [copy](const SourceArray& data, const std::string& label, auto& sort)
        {
            measureSortTime(label, data, copy, sort);
        };

        // Evaluate the performance of standard arrays
        evaluateCollection(random, sorted, reversed, organPipe, rotated, "std::array", sequentialSort, parallelSort, prepareAndMeasureSortTime);
    }
    catch (const std::exception& e)
    {
        std::cerr << "Error evaluating std::array performance: " << e.what() << '\n';
    }
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
    try
    {
        auto copy = [](const std::vector<int>& data)
        {
            return data; // Return copy of the vector
        };

        // Sequential sorting function for standard vectors
        auto sequentialSort = [](std::vector<int>& data)
        {
            std::sort(std::execution::seq, data.begin(), data.end());
        };

        // Parallel sorting function for standard vectors
        auto parallelSort = [](std::vector<int>& data)
        {
            std::sort(std::execution::par, data.begin(), data.end());
        };

        // Prepare and measure sorting time for standard vectors
        auto prepareAndMeasureSortTime = [copy](const SourceArray& data, const std::string& label, auto& sort)
        {
            std::vector dataCopy(data.begin(), data.end()); // Create a copy of the data
            measureSortTime(label, dataCopy, copy, sort);   // Measure sorting time
        };

        // Evaluate the performance of standard vectors
        evaluateCollection(random, sorted, reversed, organPipe, rotated, "std::vector", sequentialSort, parallelSort, prepareAndMeasureSortTime);
    }
    catch (const std::exception& e)
    {
        std::cerr << "Error evaluating std::vector performance: " << e.what() << '\n';
    }
}
