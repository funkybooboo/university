#pragma once

#include "Pattern.hpp"

class PatternPulsar final : public Pattern
{
public:
    PatternPulsar() = default;

    [[nodiscard]] std::uint8_t getSizeX() const override {
        return 13;
    }

    [[nodiscard]] std::uint8_t getSizeY() const override {
        return 13;
    }

    [[nodiscard]] bool getCell(std::uint8_t x, std::uint8_t y) const override;
};
