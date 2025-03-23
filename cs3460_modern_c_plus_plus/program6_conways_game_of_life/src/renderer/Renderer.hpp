#pragma once

#include "../LifeSimulator.hpp"

/**
 * @brief Abstract base class for rendering the LifeSimulator.
 *
 * This class provides an interface for rendering the simulation state
 * to different outputs (e.g., console, graphical window).
 */
class Renderer
{
  public:
    virtual ~Renderer() = default; // Default destructor

    /**
     * @brief Renders the current state of the LifeSimulator.
     *
     * This pure virtual function must be implemented by derived classes
     * to display the simulation according to their specific rendering method.
     *
     * @param simulation The LifeSimulator object whose state is to be rendered.
     */
    virtual void render(const LifeSimulator& simulation) = 0;
};
