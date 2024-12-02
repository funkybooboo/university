## RANDOM NUMBERS

### Basics
Legacy generation is with rand and srand. We don't use them and instead use the random standard library. It provides the ability to do variaous distributions (normal, uniform, exponential, poisson, etc.).
Because we're using a higher level kind of random number generation, we have different components to use:

- **Device** - This is the non-deteriministic uniform random number generator. Very low-level and is the start of the generation.
- **Engine** - A psuedo random number generator that uses the device to determine its numbers.
- **Distribution** - Based on the type, uses the engine to pick out a random number that conforms to the kind of distribution used. This allows for more control over what random numbers we want to get out of the generator.

### More on the Device
This is a *bit* generator. It can be a piece of hardware or software. If it's software then it is still psuedo-random. This is something we use to seed our engine for better picked random numbers.
Implements the () operator
- Updates internal state
- Returns a generated value

### More on the Engine
Produces some psuedo-random numbers from an initial seed. C++ provides some different kinds of engines for better number choices based on the purpose of the random numbers. Some examples are: Linear Congrueital, Mersene Twister, and Subtract with Carry. The engine will also implement the () operator in the same way the device does.

### More on the Distribution
This is where we get our actual random numbers that we can use in our program. We have a lot of options for this and can apply whichever one fits the problem best. 
The Categories (There are several of each kind):
- Uniform
- Bernoulli
- Poisson
- Normal
- Sampling

### Code Examples and Random Shuffling
```c++
#include <random>
#include <algorithm>

// Random Device
std::random_device rd;

for (int i = 0; i < 10; i++)
{
    std::cout << rd() << std::endl;
}

// Random Engine
std::random_device rd;
std::default_random_engine engine{rd()}; // Can change to be a different kind of engine

for (int i = 0; i < 10; i++)
{
    std::cout << engine() << std::endl;
}

// Random Distribution Example
std::random_device rd;
std::default_random_engine engine{rd()};
std::uniform_real_distribution<double> dist(0.0, 1.0); // Can change to be a different kind of distribution

for (int i = 0; i < 10; i++)
{
    std::cout << dist(engine) << std::endl;
}

// Random Shuffling

/*
We can use the engine to help in randomly shuffling a container. std::shuffle comes from the algorithm library and shuffles a container, using the generator we can add some psuedo-randomness to the shuffling.
*/
std::vector<int> v = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
std::array<int, 10> v2 = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

std::random_device rd;
std::mt19937 generator{rd()}; // Different kind of engine example

std::shuffle(v.begin(), v.end(), generator);
std::shuffle(v2.begin(), v2.end(), generator);

// Alternatively using ranges
std::ranges::shuffle(v, generator);
std::ranges::shuffle(v2, generator);

for (auto number : v)
{
    std::cout << number << " ";
}
std::cout << std::endl;

for (auto number : v2)
{
    std::cout << number << " ";
}
std::cout << std::endl;
```