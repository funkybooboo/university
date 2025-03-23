#include "PatternBlock.hpp"

/**
 * @brief Constructs a PatternBlock object.
 *
 * Initializes the Block pattern grid with a predefined configuration.
 */
PatternBlock::PatternBlock() :
    m_sizeX(2), // Width of the Block pattern
    m_sizeY(2)  // Height of the Block pattern
{
    // Initialize the grid with the Block pattern configuration
    m_grid = {
        { true, true }, // Row 1: Both cells are alive
        { true, true }  // Row 2: Both cells are alive
    };
}

/**
 * @brief Gets the state of a specific cell in the Block pattern.
 *
 * @param x The x-coordinate of the cell.
 * @param y The y-coordinate of the cell.
 * @return True if the cell is alive, false if it is dead or out of bounds.
 */
[[nodiscard]] bool PatternBlock::getCell(const std::uint8_t x, const std::uint8_t y) const
{
    // Check for out-of-bounds coordinates
    if (x >= m_sizeX || y >= m_sizeY)
    {
        return false; // Return dead cell if out of bounds
    }
    return m_grid[y][x]; // Return the state of the specified cell
}
