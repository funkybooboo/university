## STD::ARRAY

### Basics
Part of the standard library array. These are a fixed size container that can hold a variety of types, including primitive types. (That is where C++ arrays differ from Java's).

You can also find the size of it with the operator .size()
```c++
// Two ways to initialize a std::array in C++
#include <array>
// Initialize the array and then access the locations and set them to a specific integer.
std::array<int, 4> primes;
primes[0] = 2;
primes[1] = 3;
primes[2] = 5;
primes[3] = 7;

// Using an initializer list to setup the array.
std::array<int, 4> primes { 2, 3, 5, 7 };

// Find the size of an array
std::cout << "Size: " << primes.size() << std::endl;
```

By default they are single dimensioned but you can make a multi-dimensional one like so.
```c++
// By wrapping a std::array in another std::array, you can make something akin to that of a matrix.
std::array<std::array<int, 3>, 3> table3
    { {
        { 0, 1, 2 },
        { 3, 4, 5 },
        { 6, 7, 8 }
    } };
```

### Type inference with Arrays
Lets take 'auto primes = {2, 3, 5, 7};' as an example. This will not be inferred as an array by auto, but isntead as a "std::initializer_list". We can use this to create a standard array as such:
```c++
#include <array>
auto primes = {2, 3, 5, 7};

std::array primes{2, 3, 5, 7};
```