#include "PatternBlock.hpp"

PatternBlock::PatternBlock() :
    m_sizeX(2),
    m_sizeY(2)
{
    m_grid = { { true, true },
               { true, true } };
}

[[nodiscard]] bool PatternBlock::getCell(const std::uint8_t x, const std::uint8_t y) const
{
    if (x >= m_sizeX || y >= m_sizeY)
    {
        return false;
    }
    return m_grid[y][x];
}