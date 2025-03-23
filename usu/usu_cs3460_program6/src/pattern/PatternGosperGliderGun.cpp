#include "PatternGosperGliderGun.hpp"

/**
 * @brief Constructs a PatternGosperGliderGun object.
 *
 * Initializes the Gosper Glider Gun pattern grid with a predefined configuration.
 */
PatternGosperGliderGun::PatternGosperGliderGun() :
    m_sizeX(36), // Width of the Gosper Glider Gun pattern
    m_sizeY(9)   // Height of the Gosper Glider Gun pattern
{
    // Initialize the grid with the Gosper Glider Gun pattern configuration
    m_grid = {
        { false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false },
        { false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, true, false, false, false, false, false, false, false, false, false, false, false },
        { false, false, false, false, false, false, false, false, false, false, false, false, true, true, false, false, false, false, false, false, true, true, false, false, false, false, false, false, false, false, false, false, false, false, true, true },
        { false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, true, false, false, false, false, true, true, false, false, false, false, false, false, false, false, false, false, false, false, true, true },
        { true, true, false, false, false, false, false, false, false, false, true, false, false, false, false, false, true, false, false, false, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false },
        { true, true, false, false, false, false, false, false, false, false, true, false, false, false, true, false, true, true, false, false, false, false, true, false, true, false, false, false, false, false, false, false, false, false, false, false },
        { false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, true, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false },
        { false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false },
        { false, false, false, false, false, false, false, false, false, false, false, false, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false }
    };
}

/**
 * @brief Gets the state of a specific cell in the Gosper Glider Gun pattern.
 *
 * @param x The x-coordinate of the cell.
 * @param y The y-coordinate of the cell.
 * @return True if the cell is alive, false if it is dead or out of bounds.
 */
[[nodiscard]] bool PatternGosperGliderGun::getCell(const std::uint8_t x, const std::uint8_t y) const
{
    // Check for out-of-bounds coordinates
    if (x >= m_sizeX || y >= m_sizeY)
    {
        return false; // Return dead cell if out of bounds
    }
    return m_grid[y][x]; // Return the state of the specified cell
}
