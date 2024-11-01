#pragma once

#include "pattern/Pattern.hpp"

class LifeSimulator
{
public:
    LifeSimulator(std::uint8_t sizeX, std::uint8_t sizeY);

    void insertPattern(const Pattern& pattern, std::uint8_t startX, std::uint8_t startY);
    void update();

    [[nodiscard]] std::uint8_t getSizeX() const;
    [[nodiscard]] std::uint8_t getSizeY() const;
    [[nodiscard]] bool getCell(std::uint8_t x, std::uint8_t y) const;
};