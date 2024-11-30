#pragma once

#include <print>

template <typename T>
T xtoy(T x, unsigned int y)
{
    T result = 1;
    for (decltype(y) i{ 0 }; i < y; i++)
    {
        result *= x;
    }
    return result;
}

template <typename T>
void report(T data)
{
    std::print("There are {} elements in the container\n", data.size());
    for (auto item : data)
    {
        std::print("{}\n", item);
    }
}

template <typename T, unsigned int R>
void repeat(T value)
{
    for (decltype(R) i{ 0 }; i < R; i++)
    {
        std::print("{}\n", value);
    }
}

 template <typename T>
 T max(T x)
{
     std::print("max({0})\n", x);
     return x;
 }

template <typename T, typename... Ts>
T max(T x, Ts... ts)
{
    std::print("max({0}, Ts...)\n", x);
    return (x > max(ts...)) ? x : max(ts...);
}

template <typename... Ts>
unsigned int howManyTypes(Ts... ts)
{
    return sizeof...(Ts);
}
