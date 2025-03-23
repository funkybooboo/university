#include "Company.hpp"

#include <cstdint>
#include <format>
#include <iostream>
#include <memory>
#include <print>

std::string obtainThreeWayResult(std::uint32_t v1, std::uint32_t v2)
{
    std::strong_ordering order = v1 <=> v2;
    if (order == std::strong_ordering::less)
    {
        return std::format("{} <=> {} : less", v1, v2);
    }
    else if (order == std::strong_ordering::greater)
    {
        return std::format("{} <=> {} : greater", v1, v2);
    }
    else if (order == std::strong_ordering::equal)
    {
        return std::format("{} <=> {} : equal", v1, v2);
    }
    else if (order == std::strong_ordering::equivalent)
    {
        return std::format("{} <=> {} : equivalent", v1, v2);
    }
    return "";
}

int main()
{
    // Company myCompany("Dean's Company");
    // auto e1 = std::make_shared<Employee>("Karl", "Mathias", static_cast<std::uint8_t>(20));

    // myCompany += e1;
    // myCompany += std::make_shared<Employee>("Sam", "Mathias", static_cast<std::uint8_t>(15));
    // myCompany += std::make_shared<Employee>("Stephanie", "Hilpert", static_cast<std::uint8_t>(10));

    // myCompany -= myCompany.findbyName("Sam", "Mathias");

    // Employee e1("Luke", "Seamons", 0);
    // Employee e2("Lapriel", "Sanders", 0);
    // Employee e3("Chad", "Mano", 20);

    // e1 = e2 = e3;
    ////e1.operator=(e2.operator=(e3));

    // std::cout << e1.getFullName() << std::endl;

    std::cout << obtainThreeWayResult(17, 23) << std::endl;
    std::cout << obtainThreeWayResult(23, 17) << std::endl;
    std::cout << obtainThreeWayResult(17, 29) << std::endl;
    std::cout << obtainThreeWayResult(17, 17) << std::endl;

    return 0;
}
