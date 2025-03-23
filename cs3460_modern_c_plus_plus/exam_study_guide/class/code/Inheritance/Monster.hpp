#pragma once

#include "Entity.hpp"

#include <string>

class Monster : public Entity
{
  public:
    using Entity::Entity; // Inheriting constructors from Entity
    Monster(std::string name, double facing, double posX, double posY);

    virtual void update(double elapsedTime) override;

    std::string getName() { return m_name; }

  protected:
    virtual std::string getType() override { return "Monster"; }
    virtual void reportUnique() override;

  private:
    std::string m_name{ "anonymous" };
};
