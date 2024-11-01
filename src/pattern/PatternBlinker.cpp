#include "PatternBlinker.hpp"

[[nodiscard]] bool PatternBlinker::getCell(const std::uint8_t x, const std::uint8_t y) const override
{
    return y == 1; // Vertical line of three
}