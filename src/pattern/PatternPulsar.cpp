#include "PatternPulsar.hpp"

PatternPulsar::PatternPulsar() :
    m_sizeX(13),
    m_sizeY(13)
{
    m_grid = {
        { false, false, true, true, true, false, false, false, true, true, true, false, false },
        { false, false, false, false, false, false, false, false, false, false, false, false, false },
        { true, false, false, false, false, true, false, true, false, false, false, false, true },
        { true, false, false, false, false, true, false, true, false, false, false, false, true },
        { true, false, false, false, false, true, false, true, false, false, false, false, true },
        { false, false, true, true, true, false, false, false, true, true, true, false, false },
        { false, false, false, false, false, false, false, false, false, false, false, false, false },
        { false, false, true, true, true, false, false, false, true, true, true, false, false },
        { true, false, false, false, false, true, false, true, false, false, false, false, true },
        { true, false, false, false, false, true, false, true, false, false, false, false, true },
        { true, false, false, false, false, true, false, true, false, false, false, false, true },
        { false, false, false, false, false, false, false, false, false, false, false, false, false },
        { false, false, true, true, true, false, false, false, true, true, true, false, false },
    };
}

[[nodiscard]] bool PatternPulsar::getCell(const std::uint8_t x, const std::uint8_t y) const
{
    if (x >= m_sizeX || y >= m_sizeY)
    {
        return false;
    }
    return m_grid[y][x];
}