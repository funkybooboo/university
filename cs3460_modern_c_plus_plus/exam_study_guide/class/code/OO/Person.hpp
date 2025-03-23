#pragma once

#include <string>

class Person
{
  public:
    Person(std::string nameFirst, std::string nameLast);

    [[nodiscard]] std::string getFullName();
    std::string getLastName() const { return m_nameLast; }
    std::string getFirstName() const { return m_nameFirst; }

    void setFirstName(std::string name) { m_nameFirst = name; }

  private:
    std::string m_nameFirst;
    std::string m_nameLast;
};
