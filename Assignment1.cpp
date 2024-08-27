#include <iostream>
#include <stdexcept>
#include <cmath>

/**
 * @brief Calculates the n-th term of an n-bonacci sequence.
 *
 * An n-bonacci sequence is similar to the Fibonacci sequence.
 *
 * @param series What bonacci series to compute on.
 * @param n The index of the term to compute (0-based).
 *
 * @return The n-th term of the n-bonacci sequence.
 *
 * @throws std::invalid_argument if `series` is less than 2.
 */
long nbonacci(const unsigned int series, const unsigned int n)
{
    if (series < 2) throw std::invalid_argument("Series has to be at least 2");
    if (n < series) return 1;
    long k = 0;
    for (unsigned int i = 1; i <= series; i++)
    {
        k += nbonacci(series, n - i);
    }
    return k;
}

/**
 * @brief Prints the first 20 terms of an n-bonacci sequence.
 *
 * Prints the sequence with the specified title and the n-bonacci sequence.
 *
 * @param title The title to be printed before the sequence.
 * @param series What bonacci series to compute on.
 */
void printSequence(const std::string& title, const unsigned int series)
{
    constexpr unsigned short SEQUENCE_LENGTH = 20;
    std::cout << title << std::endl;
    for (unsigned int i = 0; i < SEQUENCE_LENGTH; i++)
    {
        std::cout << nbonacci(series, i) << " ";
    }
    std::cout << std::endl;
}

/**
 * @brief Computes the ratio of successive terms in an n-bonacci sequence.
 *
 * Estimates the ratio of successive terms in the sequence and prints the result
 * when the ratio converges (i.e., changes by less than 0.000001 between iterations).
 *
 * @param title The title to be printed with the computed ratio.
 * @param series What bonacci series to compute on.
 */
void computeNbonacciRatio(const std::string& title, const unsigned int series)
{
    double previousEstimate;
    double currentEstimate = 1;
    unsigned int n = series;
    do
    {
        n++;
        previousEstimate = currentEstimate;
        currentEstimate = static_cast<double>(nbonacci(series, n)) / static_cast<double>(nbonacci(series, n - 1));
    }
    while (std::abs(currentEstimate - previousEstimate) >= 0.000001);
    std::cout << title << " ratio approaches " << currentEstimate << " after " << n << " iterations" << std::endl;
}

/**
 * @brief Main function to demonstrate the n-bonacci sequence calculations and ratios.
 *
 * Prints sequences for Fibonacci, Tribonacci, Fourbonacci, and Fivebonacci sequences.
 * Computes and prints the ratio for each sequence.
 *
 * @return Exit status of the program.
 */
int main()
{
    printSequence("--- Fibonacci Sequence ---", 2);
    printSequence("--- Tribonacci Sequence ---", 3);
    printSequence("--- Fourbonacci Sequence ---", 4);
    printSequence("--- Fivebonacci Sequence ---", 5);
    std::cout << std::endl;
    computeNbonacciRatio("Fibonacci", 2);
    computeNbonacciRatio("Tribonacci", 3);
    computeNbonacciRatio("Fourbonacci", 4);
    computeNbonacciRatio("Fivebonacci", 5);
    return 0;
}
