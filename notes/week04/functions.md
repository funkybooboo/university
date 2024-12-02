## FUNCTIONS

### Basics
In C++, the compiler must know of the functions' existence before it can be called and used in the code. This means the function implementation must be above where the function call lives.
This differs from Java where you could write a static method after the function call.

An alternative to this is to use function prototypes. Where we declare the function before it's called and then define it later in the code.

### Pass-By-Value
Functions are Pass-by-Value by default. The value is copied, which is the same as in Java. And it is important to pay attention to just what the 'value' is. But with the existence of pointers, this can become more complex. The return values are also return-by-value.

In this Code Example:
- values parameter will make a copy as it is by-value.
- Same with returning values, a copy will be made.
```c++
std::array<int, 4> byTwo(std::array<int, 4> values)
{
    for (std::uint8_t i = 0; i < values.size(); i++)
    {
        values[i] *= 2;
    }
    return values;
}
```

### Default Parameter Values
We can set a default value like we can in other languages.
```c++
// We set the default value of n to 2.
std::array<int, 4> byN(std::array<int, 4> values, int n = 2) {...body...}

// We can either call this with only an array or with an array and an integer to override the default.
std::array<int, 4> primes{2,3,5,7};
auto result = byN(primes);
auto result = byN(primes, 4);
```
There can also be multiple default parameters.
```c++
// Similar to the above example, our size and initialValue can have a default like so
std::vector<int> createArray(int size = 10, int initialValue = 0) {...body...}

// Various ways to call the function by using both, one, or none of the default parameters.
auto array1 = createArray();
auto array2 = createArray(20);
auto array3 = createArray(20, 1);
```
Important to note, we cannot access the default parameter of size and set our own initialValue. 