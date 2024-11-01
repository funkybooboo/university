#pragma once

#include "Pattern.hpp"

class PatternBlinker final : public Pattern
{
public:
    PatternBlinker() = default;

    [[nodiscard]] std::uint8_t getSizeX() const override
    {
        return 1;
    }

    [[nodiscard]] std::uint8_t getSizeY() const override
    {
        return 3;
    }

    [[nodiscard]] bool getCell(std::uint8_t x, std::uint8_t y) const override;
};
