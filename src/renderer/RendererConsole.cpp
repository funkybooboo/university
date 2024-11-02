#include "RendererConsole.hpp"

#include "rlutil.h"

RendererConsole::RendererConsole(const std::uint8_t sizeX, const std::uint8_t sizeY) :
    m_sizeX(sizeX),
    m_sizeY(sizeY),
    m_previousGrid(sizeY, std::vector(sizeX, false))
{
    rlutil::cls();
    rlutil::hidecursor(); // Hide the cursor for rendering

    // Render the initial state of the grid
    for (std::uint8_t y = 0; y < m_sizeY; ++y)
    {
        for (std::uint8_t x = 0; x < m_sizeX; ++x)
        {
            const bool cell = m_previousGrid[y][x]; // Get the initial state from the simulation
            rlutil::locate(x + 1, y + 1);           // Set cursor location (1-indexed for rlutil)
            if (cell)
            {
                rlutil::setChar('*'); // Alive cell representation
            }
            else
            {
                rlutil::setChar(' '); // Dead cell representation
            }
            m_previousGrid[y][x] = cell; // Initialize the previous state
        }
    }

    rlutil::showcursor();    // Show the cursor again
    std::cout << std::flush; // Ensure buffer is flushed to the console
}

void RendererConsole::render(const LifeSimulator& simulation)
{
    // First time called: clear the screen and set up the initial state
    rlutil::hidecursor();

    // Loop through each cell in the simulator
    for (std::uint8_t y = 0; y < m_sizeY; y++)
    {
        for (std::uint8_t x = 0; x < m_sizeX; x++)
        {

            // Only update if the cell state has changed
            const bool cell = simulation.getCell(x, y);
            if (cell != m_previousGrid[y][x])
            {
                rlutil::locate(x + 1, y + 1); // Set cursor location (1-indexed for rlutil)
                if (cell)
                {
                    rlutil::setChar('*'); // Alive cell representation
                }
                else
                {
                    rlutil::setChar(' '); // Dead cell representation
                }
                m_previousGrid[y][x] = cell; // Update previous state
            }
        }
    }

    rlutil::showcursor();
    std::cout << std::flush; // Ensure buffer is flushed to the console
}