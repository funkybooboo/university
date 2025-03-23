#include "utilities.hpp"

namespace cs3460
{
    //
    // Approximation of sin:
    // https://en.wikipedia.org/wiki/Bhaskara_I's_sine_approximation_formula Valid
    // for values angles [0, 180]
    //
    double sin(double angle)
    {
        return (4 * angle * (180 - angle)) / (40500 - angle * (180 - angle));
    }

    void swap(int& x, int& y)
    {
        int temp = x;
        x = y;
        y = temp;
    }

} // namespace cs3460
