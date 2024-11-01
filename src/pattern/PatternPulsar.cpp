#include "PatternPulsar.hpp"

[[nodiscard]] bool PatternPulsar::getCell(const std::uint8_t x, const std::uint8_t y) const override
{
    return (x == 5 && y == 1) || (x == 5 && y == 2) ||
           (x == 6 && y == 1) || (x == 6 && y == 2) ||
           (x == 3 && y == 3) || (x == 4 && y == 3) ||
           (x == 5 && y == 3) || (x == 6 && y == 3) ||
           (x == 7 && y == 3) || (x == 8 && y == 4) ||
           (x == 2 && y == 5) || (x == 3 && y == 6) ||
           (x == 2 && y == 6) || (x == 1 && y == 7) ||
           (x == 0 && y == 7) || (x == 1 && y == 8);
}