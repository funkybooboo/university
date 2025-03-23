#include "Entity.hpp"

#include <cmath>
#include <iostream>
#include <print>

std::uint32_t Entity::nextId = 0;

Entity::Entity(double facing, double posX, double posY) :
    m_id(nextId++),
    m_speed(0),
    m_facing(facing),
    m_posX(posX),
    m_posY(posY)
{
}

void Entity::update(double elapsedTime)
{
    move(elapsedTime);
}

void Entity::report()
{
    std::print("--- {} Report ---\n", getType());
    std::print("id       : {}\n", m_id);
    std::print("Position : ({:.3f}, {:.3f})\n", m_posX, m_posY);
    std::print("Facing   : {}\n", m_facing);
    std::print("Speed    : {}\n", m_speed);

    // Let each derived class report on its unique properties
    reportUnique(); // Runtime polymorphism, dynamic binding

    std::cout << std::endl;
}

void Entity::move(double elapsedTime)
{
    auto vectorX = std::cos(m_facing);
    auto vectorY = std::sin(m_facing);

    m_posX += (vectorX * elapsedTime * m_speed);
    m_posY += (vectorY * elapsedTime * m_speed);
}
