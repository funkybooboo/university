
#include "LifeSimulator.hpp"
#include "pattern/PatternAcorn.hpp"
#include "pattern/PatternBlinker.hpp"
#include "pattern/PatternBlock.hpp"
#include "pattern/PatternGlider.hpp"
#include "pattern/PatternGosperGliderGun.hpp"
#include "pattern/PatternPulsar.hpp"
#include "renderer/RendererConsole.hpp"

#include <chrono>
#include <thread>

int main()
{
    constexpr std::uint8_t sizeX = 100;
    constexpr std::uint8_t sizeY = 100;
    constexpr std::uint8_t T = 100;

    LifeSimulator lifeSimulator{ sizeX, sizeY };

    lifeSimulator.insertPattern(PatternAcorn{}, 0, 0);
    lifeSimulator.insertPattern(PatternBlinker{}, 40, 10);
    lifeSimulator.insertPattern(PatternBlock{}, 10, 10);
    lifeSimulator.insertPattern(PatternGlider{}, 10, 40);
    lifeSimulator.insertPattern(PatternGosperGliderGun{}, 50, 50);
    lifeSimulator.insertPattern(PatternPulsar{}, 20, 70);

    RendererConsole rendererConsole{ sizeX, sizeY };
    for (std::uint8_t t = 0; t < T; t++)
    {
        rendererConsole.render(lifeSimulator);
        lifeSimulator.update();
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }

    return 0;
}