#pragma once
#include <cstdint>
#include <string>

class Employee
{
  public:
    Employee(std::string nameFirst, std::string nameLast, std::uint8_t yearsOfService);

    // bool operator==(const Employee& rhs) const;
    Employee& operator=(const Employee& rhs);

    std::strong_ordering operator<=>(const Employee& rhs) const = default;

    std::string getFullName();
    std::string getNameFirst() { return m_nameFirst; }
    std::string getNameLast() { return m_nameLast; }
    std::uint8_t getYearsOfService() { return m_yearsOfService; }

  private:
    std::string m_nameFirst;
    std::string m_nameLast;
    std::uint8_t m_yearsOfService;
};
