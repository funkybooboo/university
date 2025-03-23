#include "PatternPulsar.hpp"

/**
 * @brief Constructs a PatternPulsar object.
 *
 * Initializes the Pulsar pattern grid with a predefined configuration.
 */
PatternPulsar::PatternPulsar() :
    m_sizeX(13), // Width of the Pulsar pattern
    m_sizeY(13)  // Height of the Pulsar pattern
{
    // Initialize the grid with the Pulsar pattern configuration
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

/**
 * @brief Gets the state of a specific cell in the Pulsar pattern.
 *
 * @param x The x-coordinate of the cell.
 * @param y The y-coordinate of the cell.
 * @return True if the cell is alive, false if it is dead or out of bounds.
 */
[[nodiscard]] bool PatternPulsar::getCell(const std::uint8_t x, const std::uint8_t y) const
{
    // Check for out-of-bounds coordinates
    if (x >= m_sizeX || y >= m_sizeY)
    {
        return false; // Return dead cell if out of bounds
    }
    return m_grid[y][x]; // Return the state of the specified cell
}
