#include "LifeSimulator.hpp"
#include "pattern/PatternAcorn.hpp"
#include "pattern/PatternBlinker.hpp"
#include "pattern/PatternBlock.hpp"
#include "pattern/PatternGlider.hpp"
#include "pattern/PatternGosperGliderGun.hpp"
#include "pattern/PatternPulsar.hpp"
#include "renderer/RendererConsole.hpp"

#include <chrono>
#include <iostream>
#include <thread>

/**
 * @brief Waits for the user to press Enter to continue.
 *
 * This function displays a prompt and waits for the user to
 * press the Enter key.
 */
void pressEnterToContinue()
{
    std::cout << "Press Enter to continue...";
    while (std::cin.get() != '\n')
    {
        // Loop until Enter is pressed
    }
}

/**
 * @brief Animates a pattern in the Life Simulator.
 *
 * This function sets up and runs a simulation of a specified
 * pattern for a given number of time steps. It renders the
 * pattern in the console at each step.
 *
 * @param sizeX The width of the simulation grid.
 * @param sizeY The height of the simulation grid.
 * @param T The number of time steps to simulate.
 * @param pattern The pattern to be inserted into the simulator.
 * @param x The x-coordinate where the pattern will be placed.
 * @param y The y-coordinate where the pattern will be placed.
 * @param title The title displayed before the animation starts.
 */
void animate(
    const std::uint8_t sizeX,
    const std::uint8_t sizeY,
    const std::uint8_t T,
    const Pattern& pattern,
    const std::uint8_t x,
    const std::uint8_t y,
    const std::string& title)
{
    std::cout << title << std::endl;
    pressEnterToContinue();

    LifeSimulator lifeSimulator{ sizeX, sizeY };     // Create the simulator with specified size
    lifeSimulator.insertPattern(pattern, x, y);      // Insert the pattern at the specified coordinates
    RendererConsole rendererConsole{ sizeX, sizeY }; // Create a console renderer

    for (std::uint8_t t = 0; t < T; t++)
    {
        rendererConsole.render(lifeSimulator);                       // Render the current state of the simulator
        lifeSimulator.update();                                      // Update the simulator for the next time step
        std::this_thread::sleep_for(std::chrono::milliseconds(100)); // Wait before the next update
    }
}

/**
 * @brief The main function to run the Life Simulator.
 *
 * This function initializes the simulator and animates various
 * patterns including Acorn, Blinker, Block, Glider, Gosper Glider Gun,
 * and Pulsar.
 *
 * @return Exit status of the program.
 */
int main()
{
    constexpr std::uint8_t sizeX = 50; // Width of the simulation grid
    constexpr std::uint8_t sizeY = 50; // Height of the simulation grid
    constexpr std::uint8_t T = 50;     // Number of time steps to simulate

    // Animate each pattern with the specified parameters
    animate(sizeX, sizeY, T, PatternAcorn{}, 10, 10, "--- Acorn ---");
    animate(sizeX, sizeY, T, PatternBlinker{}, 10, 10, "--- Blinker ---");
    animate(sizeX, sizeY, T, PatternBlock{}, 10, 10, "--- Block ---");
    animate(sizeX, sizeY, T, PatternGlider{}, 10, 10, "--- Glider ---");
    animate(sizeX, sizeY, T, PatternGosperGliderGun{}, 10, 10, "--- Gosper Glider Gun ---");
    animate(sizeX, sizeY, T, PatternPulsar{}, 10, 10, "--- Pulsar ---");

    return 0;
}
