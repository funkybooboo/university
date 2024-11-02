#include "PatternGlider.hpp"

PatternGlider::PatternGlider() :
    m_sizeX(3),
    m_sizeY(3)
{
    m_grid = { { false, false, true },
               { true, false, true },
               { false, true, true } };
}

[[nodiscard]] bool PatternGlider::getCell(const std::uint8_t x, const std::uint8_t y) const
{
    if (x >= m_sizeX || y >= m_sizeY)
    {
        return false;
    }
    return m_grid[y][x];
}