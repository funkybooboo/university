#include "Person.hpp"

#include <format>

Person::Person(std::string nameFirst, std::string nameLast) :
    m_nameLast(nameLast),
    m_nameFirst(nameFirst)
{
}

std::string Person::getFullName()
{
    return std::format("{} {}", m_nameLast, m_nameFirst);
}
