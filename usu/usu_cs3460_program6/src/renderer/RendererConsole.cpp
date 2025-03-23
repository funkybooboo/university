#include "RendererConsole.hpp"

/**
 * @brief Constructs a RendererConsole with specified dimensions.
 *
 * Initializes the console for rendering and draws the initial borders and state.
 *
 * @param sizeX The width of the rendering area.
 * @param sizeY The height of the rendering area.
 */
RendererConsole::RendererConsole(const std::uint8_t sizeX, const std::uint8_t sizeY) :
    m_sizeX(sizeX),
    m_sizeY(sizeY),
    m_previousGrid(sizeY, std::vector<bool>(sizeX, false)) // Initialize previous grid to dead cells
{
    rlutil::cls();        // Clear the console
    rlutil::hidecursor(); // Hide the cursor for cleaner rendering

    // Draw the border around the grid
    for (std::uint8_t x = 0; x < m_sizeX + 2; ++x)
    {
        rlutil::locate(x + 1, 1); // Top border
        rlutil::setChar('-');
        rlutil::locate(x + 1, m_sizeY + 2); // Bottom border
        rlutil::setChar('-');
    }
    for (std::uint8_t y = 1; y <= m_sizeY; ++y)
    {
        rlutil::locate(1, y + 1); // Left border
        rlutil::setChar('|');
        rlutil::locate(m_sizeX + 2, y + 1); // Right border
        rlutil::setChar('|');
    }

    // Render the initial state of the grid
    for (std::uint8_t y = 0; y < m_sizeY; ++y)
    {
        for (std::uint8_t x = 0; x < m_sizeX; ++x)
        {
            const bool cell = m_previousGrid[y][x]; // Get the initial state
            rlutil::locate(x + 2, y + 2);           // Adjust cursor location for borders
            if (cell)
            {
                rlutil::setChar('*'); // Alive cell representation
            }
            else
            {
                rlutil::setChar(' '); // Dead cell representation
            }
            m_previousGrid[y][x] = cell; // Initialize previous state
        }
    }

    rlutil::showcursor();    // Show the cursor again
    std::cout << std::flush; // Ensure buffer is flushed to the console
}

/**
 * @brief Renders the current state of the LifeSimulator.
 *
 * Updates the console display with the current state of the simulation,
 * only refreshing cells that have changed.
 *
 * @param simulation The LifeSimulator object whose state is to be rendered.
 */
void RendererConsole::render(const LifeSimulator& simulation)
{
    rlutil::hidecursor(); // Hide the cursor for rendering

    // Loop through each cell in the simulator
    for (std::uint8_t y = 0; y < m_sizeY; y++)
    {
        for (std::uint8_t x = 0; x < m_sizeX; x++)
        {
            // Check if the cell state has changed
            if (const bool cell = simulation.getCell(x, y); cell != m_previousGrid[y][x])
            {
                rlutil::locate(x + 2, y + 2); // Adjust cursor location for borders
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

    rlutil::showcursor();    // Show the cursor again
    std::cout << std::flush; // Ensure buffer is flushed to the console
}
