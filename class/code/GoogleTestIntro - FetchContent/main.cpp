#include "utilities.hpp"

#include <array>
#include <iostream>

//
// Demonstration of using Google Test for unit testing
//
int main()
{

    std::cout << "Pi estimate: " << cs3460::PI << std::endl;
    std::cout << "Golden ratio estimate: " << cs3460::GOLDEN_RATIO << std::endl;

    for (int angle = 0; angle <= 180; angle++)
    {
        std::cout << "sin(" << angle << ") = " << cs3460::sin(angle) << std::endl;
    }

    return 0;
}
