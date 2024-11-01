#pragma once

#include "Pattern.hpp"

class PatternGosperGliderGun final : public Pattern
{
public:
    PatternGosperGliderGun() = default;

    [[nodiscard]] std::uint8_t getSizeX() const override
    {
        return 38;
    }

    [[nodiscard]] std::uint8_t getSizeY() const override
    {
        return 9;
    }

    [[nodiscard]] bool getCell(std::uint8_t x, std::uint8_t y) const override;
};
