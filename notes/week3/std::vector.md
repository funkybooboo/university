## STD::VECTOR

### Basics
These can be found in the libary vector, and are a dynamically sized container. You can use the same .size() operator to find the size here as well. Despite being dynamic, the vector will keep track of and know its own size.

```c++
#include <vector>

// Vector comes with a unique operator .push_back(). This is how you fill the vector without initializing any size as seen below.
std::vector<int> primes;
primes.push_back(2);
primes.push_back(3);
primes.push_back(5);
primes.push_back(7);

// Can initialize it very similarly as you can a standard array as seen with these two examples.

// This option allows you to start with 4 positions ready for you to fill out by doing (4) at the end.
std::vector<int> primes(4);
primes[0] = 2;
primes[1] = 3;
primes[2] = 5;
primes[3] = 7;

// Using an initializer list to initialize the vector.
std::vector<int> primes { 2, 3, 5, 7 };
```

### Type inference with Vectors
Lets take 'auto primes = {2, 3, 5, 7};' as an example. This will not be inferred as an array by auto, but isntead as a "std::initializer_list". We can use this to create a standard vector as such:
```c++
#include <vector>
auto primes = {2, 3, 5, 7};

std::vector primes1 = primes;
std::vector primes{2, 3, 5, 7};
```

You'll notice that this is the same as a standard array, and that is correct. Both vectors and arrays can be initialized in very similar ways and have a lot of overlap in them. The most important difference is that arrays are **statically sized** and vectors are **dynamically sized**.