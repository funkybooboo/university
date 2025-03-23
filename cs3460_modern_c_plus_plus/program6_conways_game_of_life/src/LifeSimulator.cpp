#include "LifeSimulator.hpp"

/**
 * @brief Constructs a LifeSimulator object with specified dimensions.
 *
 * Initializes the grid with dead cells.
 *
 * @param sizeX The width of the simulation grid.
 * @param sizeY The height of the simulation grid.
 */
LifeSimulator::LifeSimulator(const std::uint8_t sizeX, const std::uint8_t sizeY) :
    m_sizeX(sizeX),
    m_sizeY(sizeY),
    m_grid(sizeY, std::vector<bool>(sizeX, false)) // Initialize grid with dead cells
{
}

/**
 * @brief Inserts a pattern into the grid at specified coordinates.
 *
 * This function checks if the coordinates are within bounds
 * and then inserts the pattern into the grid.
 *
 * @param pattern The pattern to insert.
 * @param startX The x-coordinate to start inserting the pattern.
 * @param startY The y-coordinate to start inserting the pattern.
 */
void LifeSimulator::insertPattern(const Pattern& pattern, const std::uint8_t startX, const std::uint8_t startY)
{
    // Check if the starting coordinates are out of bounds
    if (startX >= m_sizeX || startY >= m_sizeY)
    {
        return; // Out of bounds, do nothing
    }

    // Insert the pattern into the grid
    for (std::uint8_t y = 0; y < pattern.getSizeY(); y++)
    {
        for (std::uint8_t x = 0; x < pattern.getSizeX(); x++)
        {
            // Check if the pattern fits within the grid bounds
            if (startX + x >= m_sizeX || startY + y >= m_sizeY)
            {
                continue; // Skip out-of-bounds cells
            }
            // Insert the cell state from the pattern into the grid
            m_grid[startY + y][startX + x] = pattern.getCell(x, y);
        }
    }
}

/**
 * @brief Updates the grid to the next generation according to the rules of the game.
 *
 * This function calculates the next state of the grid based on the current state
 * and the number of live neighbors for each cell.
 */
void LifeSimulator::update()
{
    // Create a new grid to hold the next generation
    std::vector<std::vector<bool>> nextGrid(m_sizeY, std::vector<bool>(m_sizeX, false));

    for (std::uint8_t y = 0; y < m_sizeY; y++)
    {
        for (std::uint8_t x = 0; x < m_sizeX; x++)
        {
            // Count the live neighbors
            int liveNeighbors = 0;
            for (int dy = -1; dy <= 1; dy++)
            {
                for (int dx = -1; dx <= 1; dx++)
                {
                    // Skip the cell itself
                    if (dx == 0 && dy == 0)
                    {
                        continue;
                    }
                    // Calculate the neighbor's coordinates
                    const auto neighborX = static_cast<std::int8_t>(x + dx);
                    const auto neighborY = static_cast<std::int8_t>(y + dy);

                    // Check if the neighbor is within bounds
                    if (neighborX < 0 || neighborX >= m_sizeX || neighborY < 0 || neighborY >= m_sizeY)
                    {
                        continue; // Skip out-of-bounds neighbors
                    }
                    // Count live neighbors
                    if (m_grid[neighborY][neighborX])
                    {
                        liveNeighbors++;
                    }
                }
            }

            // Apply the rules of the Game of Life
            if (m_grid[y][x]) // If the cell is alive
            {
                // Cell dies if it has fewer than 2 or more than 3 live neighbors
                nextGrid[y][x] = (liveNeighbors == 2 || liveNeighbors == 3);
            }
            else // If the cell is dead
            {
                // Cell becomes alive if it has exactly 3 live neighbors
                nextGrid[y][x] = (liveNeighbors == 3);
            }
        }
    }

    // Update the grid with the new generation
    m_grid = std::move(nextGrid);
}

/**
 * @brief Gets the state of a specific cell in the grid.
 *
 * @param x The x-coordinate of the cell.
 * @param y The y-coordinate of the cell.
 * @return True if the cell is alive, false if it is dead.
 */
[[nodiscard]] bool LifeSimulator::getCell(const std::uint8_t x, const std::uint8_t y) const
{
    // Check if the coordinates are out of bounds
    if (x >= m_sizeX || y >= m_sizeY)
    {
        return false; // Out of bounds, assume dead
    }
    return m_grid[y][x]; // Return the cell's state
}
