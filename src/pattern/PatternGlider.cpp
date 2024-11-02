#include "PatternGlider.hpp"

[[nodiscard]] bool PatternGlider::getCell(const std::uint8_t x, const std::uint8_t y) const
{
    return (x == 0 && y == 1) ||
           (x == 1 && y == 2) ||
           (x == 2 && y == 0) ||
           (x == 2 && y == 1) ||
           (x == 2 && y == 2);
}