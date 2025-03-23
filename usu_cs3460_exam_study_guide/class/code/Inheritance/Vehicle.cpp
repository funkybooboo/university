#include "Vehicle.hpp"

#include <iostream>

Vehicle::Vehicle(Color color, double facing, double posX, double posY) :
    m_color(color),
    Entity(facing, posX, posY)
{
}

void Vehicle::update(double elapsedTime)
{
    // Have it start moving
    if (m_speed == 0)
    {
        m_speed = 0.75;
    }

    // Let the parent class perform its update
    Entity::update(elapsedTime);
}

void Vehicle::reportUnique()
{
    std::cout << "Color    : ";
    switch (m_color)
    {
        case Color::Red:
            std::cout << "Red";
            break;
        case Color::Blue:
            std::cout << "Blue";
            break;
        case Color::Silver:
            std::cout << "Silver";
            break;
        case Color::White:
            std::cout << "White";
            break;
    }

    std::cout << std::endl;
}
