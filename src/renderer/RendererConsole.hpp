#pragma once

#include "Renderer.hpp"

class RendererConsole final : public Renderer
{
public:
    void render(const LifeSimulator& simulation) override;
};