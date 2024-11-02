#include "PatternAcorn.hpp"

PatternAcorn::PatternAcorn() :
    m_sizeX(7),
    m_sizeY(3)
{
    m_grid = {
        { false, true, false, false, false, false, false },
        { false, false, false, true, false, false, false },
        { true, true, false, false, true, true, true },
    };
}

[[nodiscard]] bool PatternAcorn::getCell(const std::uint8_t x, const std::uint8_t y) const
{
    if (x >= m_sizeX || y >= m_sizeY)
    {
        return false;
    }
    return m_grid[y][x];
}