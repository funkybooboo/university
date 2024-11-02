#pragma once

#include "Pattern.hpp"

#include <vector>

class PatternBlock final : public Pattern
{
  public:
    PatternBlock();

    [[nodiscard]] std::uint8_t getSizeX() const override
    {
        return m_sizeX;
    }

    [[nodiscard]] std::uint8_t getSizeY() const override
    {
        return m_sizeY;
    }

    [[nodiscard]] bool getCell(std::uint8_t x, std::uint8_t y) const override;

  private:
    std::uint8_t m_sizeX;
    std::uint8_t m_sizeY;
    std::vector<std::vector<bool>> m_grid;
};
