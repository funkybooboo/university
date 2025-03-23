#pragma once

#include "Entity.hpp"

class Vehicle : public Entity
{
  public:
    enum class Color
    {
        Red,
        Blue,
        Silver,
        White,
    };

    Vehicle(Color color, double facing, double posX, double posY);

    virtual void update(double elapsedTime) override;

  protected:
    virtual std::string getType() override { return "Vehicle"; }
    virtual void reportUnique() override;

  private:
    Color m_color;
};
