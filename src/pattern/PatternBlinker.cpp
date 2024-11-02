#include "PatternBlinker.hpp"

[[nodiscard]] bool PatternBlinker::getCell(const std::uint8_t x, const std::uint8_t y) const
{
    return y == 1 && (x == 0 || x == 1 || x == 2); // Vertical line of three
}