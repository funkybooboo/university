#include "Monster.hpp"

#include <iostream>
#include <print>

Monster::Monster(std::string name, double facing, double posX, double posY) :
    m_name(name),
    Entity(facing, posX, posY)
{
}

void Monster::update(double elapsedTime)
{
    // If it was moving, have it stop, otherwise have it move
    // Have it start moving
    if (m_speed == 0)
    {
        m_speed = 0.25;
    }
    else
    {
        m_speed = 0.0;
    }

    // Let the parent class perform its update
    Entity::update(elapsedTime);
}

void Monster::reportUnique()
{
    std::print("Name:      {}\n", m_name);
}
