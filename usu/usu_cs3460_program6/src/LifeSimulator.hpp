#pragma once

#include "pattern/Pattern.hpp"

#include <cstdint>
#include <vector>

/**
 * @brief Represents the Life Simulator.
 *
 * This class simulates a grid of cells for Conway's Game of Life,
 * allowing patterns to be inserted and updated over time.
 */
class LifeSimulator
{
  public:
    /**
     * @brief Constructs a LifeSimulator object with specified dimensions.
     *
     * @param sizeX The width of the simulation grid.
     * @param sizeY The height of the simulation grid.
     */
    LifeSimulator(std::uint8_t sizeX, std::uint8_t sizeY);

    /**
     * @brief Inserts a pattern into the grid at specified coordinates.
     *
     * @param pattern The pattern to insert.
     * @param startX The x-coordinate to start inserting the pattern.
     * @param startY The y-coordinate to start inserting the pattern.
     */
    void insertPattern(const Pattern& pattern, std::uint8_t startX, std::uint8_t startY);

    /**
     * @brief Updates the grid to the next state according to the rules of the game.
     *
     * This function calculates the next generation of cells based on
     * the current state of the grid.
     */
    void update();

    /**
     * @brief Gets the width of the simulation grid.
     *
     * @return The width of the grid.
     */
    [[nodiscard]] std::uint8_t getSizeX() const { return m_sizeX; }

    /**
     * @brief Gets the height of the simulation grid.
     *
     * @return The height of the grid.
     */
    [[nodiscard]] std::uint8_t getSizeY() const { return m_sizeY; }

    /**
     * @brief Gets the state of a specific cell in the grid.
     *
     * @param x The x-coordinate of the cell.
     * @param y The y-coordinate of the cell.
     * @return True if the cell is alive, false if it is dead.
     */
    [[nodiscard]] bool getCell(std::uint8_t x, std::uint8_t y) const;

  private:
    std::uint8_t m_sizeX{};                  ///< The width of the grid.
    std::uint8_t m_sizeY{};                  ///< The height of the grid.
    std::vector<std::vector<bool>> m_grid{}; ///< The grid representing cell states.
};
