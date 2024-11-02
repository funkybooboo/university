#include "PatternBlinker.hpp"

PatternBlinker::PatternBlinker() :
    m_sizeX(1),
    m_sizeY(3)
{
}

[[nodiscard]] bool PatternBlinker::getCell(const std::uint8_t x, const std::uint8_t y) const
{
    if (x >= m_sizeX || y >= m_sizeY)
    {
        return false;
    }
    return true;
}