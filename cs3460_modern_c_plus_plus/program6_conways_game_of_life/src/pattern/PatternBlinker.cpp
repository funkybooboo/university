#include "PatternBlinker.hpp"

/**
 * @brief Constructs a PatternBlinker object.
 *
 * Initializes the Blinker pattern with its dimensions.
 */
PatternBlinker::PatternBlinker() :
    m_sizeX(1), // Width of the Blinker pattern
    m_sizeY(3)  // Height of the Blinker pattern
{
    // The Blinker pattern is a vertical line of alive cells
}

/**
 * @brief Gets the state of a specific cell in the Blinker pattern.
 *
 * @param x The x-coordinate of the cell (should be 0 for Blinker).
 * @param y The y-coordinate of the cell (0, 1, or 2 for the Blinker).
 * @return True if the cell is alive, false if it is dead or out of bounds.
 */
[[nodiscard]] bool PatternBlinker::getCell(const std::uint8_t x, const std::uint8_t y) const
{
    // Check for out-of-bounds coordinates
    if (x >= m_sizeX || y >= m_sizeY)
    {
        return false; // Return dead cell if out of bounds
    }
    return true; // All cells in the Blinker pattern are alive
}
