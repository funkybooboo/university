#include "utilities.hpp"

#include <format>
#include <iostream>

//
// Demonstration of using separate declaration (header) and implementation files
//
int main()
{

    std::cout << std::format("Pi estimate: {}\n", PI);
    std::cout << std::format("Golden ratio estimate: {}\n", GOLDEN_RATIO);

    for (int angle = 0; angle <= 180; angle++)
    {
        std::cout << std::format("sin({:3}) = {:8.8f}\n", angle, sin(angle));
    }

    return 0;
}
