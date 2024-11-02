#include "LifeSimulator.hpp"

LifeSimulator::LifeSimulator(const std::uint8_t sizeX, const std::uint8_t sizeY) :
    m_sizeX(sizeX),
    m_sizeY(sizeY),
    m_grid(sizeY, std::vector(sizeX, false))
{
}

void LifeSimulator::insertPattern(const Pattern& pattern, const std::uint8_t startX, const std::uint8_t startY)
{
    // Check if the starting coordinates are out of bounds
    if (startX >= m_sizeX || startY >= m_sizeY)
    {
        return;
    }

    // Insert the pattern into the grid
    for (std::uint8_t y = 0; y < pattern.getSizeY(); y++)
    {
        for (std::uint8_t x = 0; x < pattern.getSizeX(); x++)
        {
            if (startX + x >= m_sizeX || startY + y >= m_sizeY)
            {
                continue;
            }
            m_grid[startY + y][startX + x] = pattern.getCell(x, y); // Cannot assign to rvalue
        }
    }
}

void LifeSimulator::update()
{
    // Create a new grid to hold the next generation
    std::vector nextGrid(m_sizeY, std::vector(m_sizeX, false));

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
                        continue;
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
                if (liveNeighbors < 2 || liveNeighbors > 3)
                {
                    nextGrid[y][x] = false; // Dies
                }
                else
                {
                    nextGrid[y][x] = true; // Lives on
                }
            }
            else // If the cell is dead
            {
                if (liveNeighbors == 3)
                {
                    nextGrid[y][x] = true; // Becomes alive
                }
                else
                {
                    nextGrid[y][x] = false; // Stays dead
                }
            }
        }
    }

    // Update the grid with the new generation
    m_grid = std::move(nextGrid);
}

[[nodiscard]] bool LifeSimulator::getCell(const std::uint8_t x, const std::uint8_t y) const
{
    // Check if the starting coordinates are out of bounds
    if (x >= m_sizeX || y >= m_sizeY)
    {
        return false;
    }
    return m_grid[y][x];
}
