#include "PatternAcorn.hpp"

[[nodiscard]] bool PatternAcorn::getCell(const std::uint8_t x, const std::uint8_t y) const override {
    return (x == 1 && y == 0) ||
           (x == 2 && y == 1) ||
           (x == 0 && y == 2) ||
           (x == 1 && y == 2) ||
           (x == 2 && y == 2) ||
           (x == 1 && y == 3);
}