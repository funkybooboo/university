#pragma once

#include <cmath>

namespace cs3460
{

    const double PI = 355.0 / 113.0;
    const double GOLDEN_RATIO = (1.0 + std::sqrt(5.0)) / 2.0;

    double sin(double angle);
    void swap(int& x, int& y);

} // namespace cs3460