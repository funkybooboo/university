
#include "LifeSimulator.hpp"
#include "pattern/PatternAcorn.hpp"
#include "pattern/PatternBlinker.hpp"
#include "pattern/PatternBlock.hpp"
#include "pattern/PatternGlider.hpp"
#include "pattern/PatternGosperGliderGun.hpp"
#include "pattern/PatternPulsar.hpp"
#include "renderer/RendererConsole.hpp"

int main()
{
    LifeSimulator lifeSimulator{100, 100};

    lifeSimulator.insertPattern(PatternAcorn{}, 0, 0);
    lifeSimulator.insertPattern(PatternBlinker{}, 40, 10);
    lifeSimulator.insertPattern(PatternBlock{}, 10, 10);
    lifeSimulator.insertPattern(PatternGlider{}, 10, 40);
    lifeSimulator.insertPattern(PatternGosperGliderGun{}, 50, 50);
    lifeSimulator.insertPattern(PatternPulsar{}, 20, 70);

    RendererConsole rendererConsole{};
    rendererConsole.render(lifeSimulator);

    return 0;
}