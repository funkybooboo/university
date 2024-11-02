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
#include <limits>
#include <thread>

void pressEnterToContinue()
{
    std::cout << "Press Enter to continue...";
    std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
}

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

    LifeSimulator lifeSimulator{ sizeX, sizeY };
    lifeSimulator.insertPattern(pattern, x, y);
    RendererConsole rendererConsole{ sizeX, sizeY };

    for (std::uint8_t t = 0; t < T; t++)
    {
        rendererConsole.render(lifeSimulator);
        lifeSimulator.update();
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }
}

int main()
{
    constexpr std::uint8_t sizeX = 50;
    constexpr std::uint8_t sizeY = 50;
    constexpr std::uint8_t T = 50;

    animate(sizeX, sizeY, T, PatternAcorn{}, 10, 10, "--- Acorn ---");

    animate(sizeX, sizeY, T, PatternBlinker{}, 10, 10, "--- Blinker ---");

    animate(sizeX, sizeY, T, PatternBlock{}, 10, 10, "--- Block ---");

    animate(sizeX, sizeY, T, PatternGlider{}, 10, 10, "--- Glider ---");

    animate(sizeX, sizeY, T, PatternGosperGliderGun{}, 10, 10, "--- Gosper Glider Gun ---");

    animate(sizeX, sizeY, T, PatternPulsar{}, 10, 10, "--- Pulsar ---");

    return 0;
}
