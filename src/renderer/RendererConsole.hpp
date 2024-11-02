#pragma once

#include "../LifeSimulator.hpp"
#include "Renderer.hpp"
#include "rlutil.h"

#include <vector>

/**
 * @brief Renders the Life Simulator output to the console.
 *
 * This class is responsible for displaying the current state of the
 * LifeSimulator in a console window.
 */
class RendererConsole final : public Renderer
{
  public:
    /**
     * @brief Constructs a RendererConsole with specified dimensions.
     *
     * @param sizeX The width of the rendering area.
     * @param sizeY The height of the rendering area.
     */
    RendererConsole(std::uint8_t sizeX, std::uint8_t sizeY);

    /**
     * @brief Clears the console when the RendererConsole object is destroyed.
     */
    ~RendererConsole() override
    {
        rlutil::cls(); // Clear the console screen
    }

    /**
     * @brief Renders the current state of the LifeSimulator.
     *
     * This function draws the simulation grid to the console.
     *
     * @param simulation The LifeSimulator object whose state is to be rendered.
     */
    void render(const LifeSimulator& simulation) override;

  private:
    std::uint8_t m_sizeX;                          ///< The width of the rendering area.
    std::uint8_t m_sizeY;                          ///< The height of the rendering area.
    std::vector<std::vector<bool>> m_previousGrid; ///< The previous state of the grid for comparison.
};
