#pragma once

#include "pattern/Pattern.hpp"

#include <cstdint>
#include <vector>

class LifeSimulator
{
  public:
    LifeSimulator(std::uint8_t sizeX, std::uint8_t sizeY);

    void insertPattern(const Pattern& pattern, std::uint8_t startX, std::uint8_t startY);
    void update();

    [[nodiscard]] std::uint8_t getSizeX() const { return m_sizeX; }
    [[nodiscard]] std::uint8_t getSizeY() const { return m_sizeY; }
    [[nodiscard]] bool getCell(std::uint8_t x, std::uint8_t y) const;

  private:
    std::uint8_t m_sizeX{};
    std::uint8_t m_sizeY{};
    std::vector<std::vector<bool>> m_grid{};
};
