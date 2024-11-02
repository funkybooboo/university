#pragma once

#include <cstdint>

class Pattern
{
  public:
    virtual ~Pattern() = default;
    [[nodiscard]] virtual std::uint8_t getSizeX() const = 0;
    [[nodiscard]] virtual std::uint8_t getSizeY() const = 0;
    [[nodiscard]] virtual bool getCell(std::uint8_t x, std::uint8_t y) const = 0;
};