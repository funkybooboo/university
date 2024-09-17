# CS3460 Program 2: Random Distributions

## Overview

This program generates and plots three different types of random distributions: Uniform, Normal, and Poisson. Each distribution is represented using a set of bins, and the frequency of random values falling into each bin is visualized using a simple text-based plot. The goal is to demonstrate how different types of random distributions behave and how they can be visualized.

## Features

- **Uniform Distribution**: Generates random integers uniformly within a specified range and bins them into evenly spaced intervals.
- **Normal Distribution**: Generates random values following a normal distribution with a specified mean and standard deviation. Values are binned over a range defined by the mean ± 4 standard deviations.
- **Poisson Distribution**: Generates random values based on the Poisson distribution with a specified average rate (mean). Binned over a range from 0 to the number of bins minus one.

## Functions

### `generateUniformDistribution`

```cpp
std::vector<DistributionPair> generateUniformDistribution(std::uint32_t howMany, std::uint32_t min, std::uint32_t max, std::uint8_t numberBins);
```

Generates `howMany` random integers uniformly distributed between `min` and `max`, and bins them into `numberBins` intervals. Returns a vector of `DistributionPair` objects representing the bin ranges and counts.

### `generateNormalDistribution`

```cpp
std::vector<DistributionPair> generateNormalDistribution(std::uint32_t howMany, float mean, float stdev, std::uint8_t numberBins);
```

Generates `howMany` random values following a normal distribution with the given `mean` and `stdev`, and bins them into `numberBins` intervals. The bin ranges cover from `mean - 4 * stdev` to `mean + 4 * stdev`. Returns a vector of `DistributionPair` objects representing the bin ranges and counts.

### `generatePoissonDistribution`

```cpp
std::vector<DistributionPair> generatePoissonDistribution(std::uint32_t howMany, std::uint8_t howOften, std::uint8_t numberBins);
```

Generates `howMany` random values following a Poisson distribution with the given `howOften` (mean rate), and bins them into `numberBins` intervals. The bin ranges are from 0 to `numberBins - 1`. Returns a vector of `DistributionPair` objects representing the bin ranges and counts.

### `plotDistribution`

```cpp
void plotDistribution(std::string title, const std::vector<DistributionPair>& distribution, const std::uint8_t maxPlotLineSize);
```

Plots the distribution of values in bins as a text-based histogram. The `title` parameter specifies the plot title. The `maxPlotLineSize` parameter determines the number of characters used to represent the bin with the maximum count.

## Building and Running the Program

### Prerequisites

- Git
- CMake (3.12 or higher recommended)
- A C++ compiler (e.g., g++, clang++, msvc)

### Build Instructions

1. **Clone the Repository**

   ```bash
   git clone https://github.com/funkybooboo/CS3460_Program2.git
   cd CS3460_Program2
   ```

2. **Create a Build Directory**

   It is a good practice to create a separate build directory:

   ```bash
   mkdir build
   cd build
   ```

3. **Configure the Project**

   Run CMake to configure the project and generate the build files:

   ```bash
   cmake ..
   ```

4. **Build the Project**

   Compile the project using your chosen build system (e.g., `make`):

   ```bash
   make
   ```

5. **Run the Program**

   Execute the compiled program:

   ```bash
   ./RandDistributions
   ```

   The program will generate and plot three different distributions: Uniform, Normal, and Poisson.

## Example Output

```
--- Uniform ---
[  0,  1] : ******
[  2,  3] : ********
[  4,  5] : *********

--- Normal ---
[ 30, 32] : ***
[ 33, 35] : **********
[ 36, 38] : ******

--- Poisson ---
[  0,  0] : ******
[  1,  1] : *
[  2,  2] : 
```
