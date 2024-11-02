#include "PatternBlock.hpp"

[[nodiscard]] bool PatternBlock::getCell(const std::uint8_t x, const std::uint8_t y) const
{
    return (x == 0 && y == 0) ||
           (x == 0 && y == 1) ||
           (x == 1 && y == 0) ||
           (x == 1 && y == 1);
}