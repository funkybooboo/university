#pragma once

#include "../LifeSimulator.hpp"
#include "Renderer.hpp"
#include "rlutil.h"

#include <vector>

class RendererConsole final : public Renderer
{
  public:
    RendererConsole(std::uint8_t sizeX, std::uint8_t sizeY);
    ~RendererConsole() override
    {
        rlutil::cls();
    }

    void render(const LifeSimulator& simulation) override;

  private:
    std::uint8_t m_sizeX;
    std::uint8_t m_sizeY;
    std::vector<std::vector<bool>> m_previousGrid;
};
