#include "Person.hpp"

#include <cstdint>
#include <iostream>
#include <memory>
#include <print>

void demoRawPointers()
{
    Person p1("Lisa", "Smith");

    std::cout << p1.getFullName() << std::endl;

    Person* p2 = new Person("John", "Spartan");

    std::cout << (*p2).getFullName() << std::endl;
    std::cout << p2->getFullName() << std::endl;

    delete p2;
}

// void report(std::shared_ptr<Person> p)
//{
//     std::print("First Name: {}\n", p->getFirstName());
//     std::print("Last Name : {}\n", p->getLastName());
// }
//
// void report(const std::unique_ptr<Person>& p)
//{
//     std::print("First Name: {}\n", p->getFirstName());
//     std::print("Last Name : {}\n", p->getLastName());
// }

void report(auto p)
{
    std::print("First Name: {}\n", p->getFirstName());
    std::print("Last Name : {}\n", p->getLastName());
}

void demoSmartPointers()
{
    std::shared_ptr<Person> p1 = std::make_shared<Person>("Lisa", "Smith");
    //    std::cout << p1->getFullName() << std::endl;
    report(p1);

    std::shared_ptr<Person> p2 = std::make_shared<Person>("John", "Spartan");
    //    std::cout << (*p2).getFullName() << std::endl;
    //    std::cout << p2->getFullName() << std::endl;

    report(p2);
}

void demoUniquePointers()
{
    std::unique_ptr<Person> p1 = std::make_unique<Person>("Lisa", "Smith");
    report(std::move(p1));
}

int main()
{
    // demoSmartPointers();
    demoUniquePointers();

    return 0;
}
