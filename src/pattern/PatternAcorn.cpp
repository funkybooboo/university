#include "PatternAcorn.hpp"

/**
 * @brief Constructs a PatternAcorn object.
 *
 * Initializes the Acorn pattern with its dimensions and sets up its initial state.
 */
PatternAcorn::PatternAcorn() :
    m_sizeX(7), // Width of the Acorn pattern
    m_sizeY(3)  // Height of the Acorn pattern
{
    // The Acorn pattern's initial configuration
    m_grid = {
        { false, true, false, false, false, false, false }, // Row 0
        { false, false, false, true, false, false, false }, // Row 1
        { true, true, false, false, true, true, true },     // Row 2
    };
}

/**
 * @brief Gets the state of a specific cell in the Acorn pattern.
 *
 * @param x The x-coordinate of the cell (0 to 6 for Acorn).
 * @param y The y-coordinate of the cell (0 to 2 for Acorn).
 * @return True if the cell is alive, false if it is dead or out of bounds.
 */
[[nodiscard]] bool PatternAcorn::getCell(const std::uint8_t x, const std::uint8_t y) const
{
    // Check for out-of-bounds coordinates
    if (x >= m_sizeX || y >= m_sizeY)
    {
        return false; // Return dead cell if out of bounds
    }
    return m_grid[y][x]; // Return the state of the requested cell
}
