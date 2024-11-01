#pragma once

#include "Pattern.hpp"

class PatternAcorn final : public Pattern
{
public:
    PatternAcorn() = default;

    [[nodiscard]] std::uint8_t getSizeX() const override
    {
        return 5;
    }

    [[nodiscard]] std::uint8_t getSizeY() const override
    {
        return 5;
    }

    [[nodiscard]] bool getCell(std::uint8_t x, std::uint8_t y) const override;
};
