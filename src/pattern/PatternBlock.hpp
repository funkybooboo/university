#pragma once

#include "Pattern.hpp"

class PatternBlock final : public Pattern
{
public:
    PatternBlock() = default;

    [[nodiscard]] std::uint8_t getSizeX() const override {
        return 2;
    }

    [[nodiscard]] std::uint8_t getSizeY() const override {
        return 2;
    }

    [[nodiscard]] bool getCell(std::uint8_t x, std::uint8_t y) const override;
};
