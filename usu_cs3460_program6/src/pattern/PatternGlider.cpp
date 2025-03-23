#include "PatternGlider.hpp"

/**
 * @brief Constructs a PatternGlider object.
 *
 * Initializes the Glider pattern grid with a predefined configuration.
 */
PatternGlider::PatternGlider() :
    m_sizeX(3), // Width of the Glider pattern
    m_sizeY(3)  // Height of the Glider pattern
{
    // Initialize the grid with the Glider pattern configuration
    m_grid = {
        { false, false, true }, // Row 1: Only the last cell is alive
        { true, false, true },  // Row 2: First and last cells are alive
        { false, true, true }   // Row 3: Last two cells are alive
    };
}

/**
 * @brief Gets the state of a specific cell in the Glider pattern.
 *
 * @param x The x-coordinate of the cell.
 * @param y The y-coordinate of the cell.
 * @return True if the cell is alive, false if it is dead or out of bounds.
 */
[[nodiscard]] bool PatternGlider::getCell(const std::uint8_t x, const std::uint8_t y) const
{
    // Check for out-of-bounds coordinates
    if (x >= m_sizeX || y >= m_sizeY)
    {
        return false; // Return dead cell if out of bounds
    }
    return m_grid[y][x]; // Return the state of the specified cell
}
