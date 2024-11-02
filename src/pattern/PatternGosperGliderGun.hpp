#pragma once

#include "Pattern.hpp"

#include <vector>

/**
 * @brief Represents the Gosper Glider Gun pattern in the Game of Life.
 *
 * This class defines the Gosper Glider Gun pattern and provides methods
 * to retrieve its size and cell states.
 */
class PatternGosperGliderGun final : public Pattern
{
  public:
    /**
     * @brief Constructs a PatternGosperGliderGun object.
     *
     * Initializes the Gosper Glider Gun pattern grid.
     */
    PatternGosperGliderGun();

    /**
     * @brief Gets the width of the pattern.
     *
     * @return The width (size in x-direction) of the pattern.
     */
    [[nodiscard]] std::uint8_t getSizeX() const override
    {
        return m_sizeX;
    }

    /**
     * @brief Gets the height of the pattern.
     *
     * @return The height (size in y-direction) of the pattern.
     */
    [[nodiscard]] std::uint8_t getSizeY() const override
    {
        return m_sizeY;
    }

    /**
     * @brief Gets the state of a specific cell in the pattern.
     *
     * @param x The x-coordinate of the cell.
     * @param y The y-coordinate of the cell.
     * @return True if the cell is alive, false if it is dead.
     */
    [[nodiscard]] bool getCell(std::uint8_t x, std::uint8_t y) const override;

  private:
    std::uint8_t m_sizeX;                  ///< The width of the Gosper Glider Gun pattern.
    std::uint8_t m_sizeY;                  ///< The height of the Gosper Glider Gun pattern.
    std::vector<std::vector<bool>> m_grid; ///< The grid representing the Gosper Glider Gun pattern.
};
