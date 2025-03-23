#include "Entity.hpp"
#include "Monster.hpp"
#include "Vehicle.hpp"

#include <iostream>
#include <memory>
#include <vector>

//
// Demonstration of inheritance and a bunch of things that go with that
//
int main()
{
    std::vector<std::shared_ptr<Entity>> entities;

    entities.push_back(std::make_shared<Monster>("Mean Green", 0, 0.5, 0.5));
    entities.push_back(std::make_shared<Monster>(3.14159 * 2, 0.0, 0.0));
    entities.push_back(std::make_shared<Vehicle>(Vehicle::Color::Blue, 3.14159, 0.25, 0.25));

    std::cout << "******** Before Update ********" << std::endl
              << std::endl;
    for (auto&& entity : entities)
    {

        entity->report();
        entity->update(0.10); // Runtime polymorphism, dynamic binding
    }

    std::cout << "******** After Update ********" << std::endl
              << std::endl;
    for (auto&& entity : entities)
    {
        entity->report();
    }

    return 0;
}
