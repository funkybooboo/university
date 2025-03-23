#pragma once

#include <cstdint>

/**
 * @brief Abstract base class for patterns in the Game of Life.
 *
 * This class defines the interface for various patterns that can
 * be used in the LifeSimulator. It requires derived classes to
 * implement methods to retrieve the size and state of the pattern.
 */
class Pattern
{
  public:
    virtual ~Pattern() = default; ///< Default destructor.

    /**
     * @brief Gets the width of the pattern.
     *
     * @return The width (size in x-direction) of the pattern.
     */
    [[nodiscard]] virtual std::uint8_t getSizeX() const = 0;

    /**
     * @brief Gets the height of the pattern.
     *
     * @return The height (size in y-direction) of the pattern.
     */
    [[nodiscard]] virtual std::uint8_t getSizeY() const = 0;

    /**
     * @brief Gets the state of a specific cell in the pattern.
     *
     * @param x The x-coordinate of the cell.
     * @param y The y-coordinate of the cell.
     * @return True if the cell is alive, false if it is dead.
     */
    [[nodiscard]] virtual bool getCell(std::uint8_t x, std::uint8_t y) const = 0;
};
