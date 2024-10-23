# Smart Pointers
Smart pointers simplify the use of pointers by doing some of the memory management for you. A smart pointer uses a raw pointer
under the hood, but automatically allocates and releases memory from the heap. A smart pointer keeps track of how many copies of it
exist. Once all copies have gone out of scope, the smart pointer releases the memory it is pointing to. A shared pointer
allows multiple copies to exist, while a unique pointer can have only copy.

# Shared Pointers
You can copy shared pointers and have multiple of them pointing to the same memory address. One memory address, one
value stored there, multiple pointers. Shared pointers keep track of when you copy them or they go out of scope. When
the last copy goes out of scope, the memory is released automatically.

You can also create a new smart pointer from a raw pointer and a deleter function. You create the deleter function and
can make it do whatever you want. This will behave like normal except that your deleter function will be called when the
last copy goes out of scope. See [Making Raw Pointers Smart](#making-raw-pointers-smart)

### Syntax
Shared Pointers are a templated type, create a shared pointer to an integer with `std::shared_ptr<int> a`. Instead of
using the `new` keyword to allocate your memory, use `std::make_shared<int>(1)`. Once you have your shared pointer, you
can use the same dereference and address operators as [raw pointers](dynamic_memory.md#raw-pointers).

See [Basic Shareds Example](#basic-shareds)
See [Shared Pointers and Arrays](#shared-pointers-and-arrays)

# Unique Pointers (Not on exam 1)
Skipping for now. Another smart pointer that can't be copied. You have to move it instead.

# Examples
Be sure to `#include <memory>`

## Basic Shareds
```cpp
std::shared_ptr<int> a = std::make_shared<int>(1);
std::shared_ptr<double> b = std::make_shared<double>(3.14159);
std::cout << "The value stored in a is " << *a << std::endl;
std::cout << "The value stored in b is " << *b << std::endl;
std::cout << "The address of a is " << &a << std::endl;
std::cout << "The address of b is " << &b << std::endl;
```

## Shared Pointers and Arrays
```cpp
// This did not work until C++20 
std::shared_ptr<int[]> primes = std::make_shared<int[]>(4);
primes[0] = 2;
primes[1] = 3;
primes[2] = 5;
primes[3] = 7;
for (int index = 0; index < 4; index++)
{
    std::cout << primes[index] << std::endl;
}
```

## Making Raw Pointers Smart
```cpp
void cleanupArray(int* p)
{
    delete[] p;
}
//. . .
std::shared_ptr<int[]> primes(new int[4]{2, 3, 5, 7}, cleanupArray); // new gives a raw pointer
```

## Preventing the Copy Penalty
```cpp
// Use a const reference to aovid copying a shared pointer
std::uint64_t arraySum(const std::shared_ptr<int[]>& data, std::uint32_t length)
{
    long total = 0;
    for (auto index = 0; index < length; index++)
    {
        total += data[index];
    }
    return total;
}
```
