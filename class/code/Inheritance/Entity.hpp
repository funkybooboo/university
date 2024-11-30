#pragma once

#include <cstdint>
#include <string>

// ------------------------------------------------------------------
//
// Demonstration of an abstract class
//
// ------------------------------------------------------------------
class Entity
{
  public:
    Entity(double facing, double posX, double posY);

    virtual void update(double elapsedTime);
    void report();

    std::uint32_t getId() { return m_id; }
    double getFacing() { return m_facing; }
    double getSpeed() { return m_speed; }
    double getPosX() { return m_posX; }
    double getPosY() { return m_posY; }

  protected:
    double m_facing; // Direction in radians
    double m_speed;  // World units per second, in the facing direction
    double m_posX;
    double m_posY;

    virtual std::string getType() = 0; // pure virtual method
    virtual void reportUnique() = 0;   // pure virtual method
    void move(double elapsedTime);

  private:
    static std::uint32_t nextId; // static, shared among all instances
    std::uint32_t m_id;          // Unique id for each instance
};
