# Translation Units
These are why we have header files and source files

# Examples

## Header and Source Class
Person.hpp
```cpp
#pragma once
#include <string>

class Person
{
    public:
        Person(std::string nameFirst, std::string nameLast, unsigned short age);

        std::string getFullName();
        std::string getNameFirst() { return m_nameFirst; }
        std::string getNameLast() { return m_nameLast; }
        unsigned short getAge() { return m_age; }
    private:
        std::string m_nameFirst;
        std::string m_nameLast;
        unsigned short m_age;
};
```

Person.cpp
```cpp
#include "Person.hpp"

Person::Person(std::string nameFirst, std::string nameLast, unsigned short age)
{
    m_nameFirst = nameFirst;
    m_nameLast = nameLast;
    m_age = age;
}

std::string Person::getFullName()
{
    return m_nameFirst + " " + m_nameLast;
}
```
