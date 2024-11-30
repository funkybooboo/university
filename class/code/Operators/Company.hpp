#pragma once

#include "Employee.hpp"

#include <memory>
#include <string>
#include <vector>

class Company
{
  public:
    Company(std::string name);

    std::string getName() { return m_name; }
    std::size_t getNumberOfEmployees() { return m_employees.size(); }
    std::shared_ptr<Employee> findbyName(std::string nameFirst, std::string nameLast);

    // Company& operator+=(const std::shared_ptr<Employee>& employee);
    Company& operator-=(const std::shared_ptr<Employee>& employee);

    friend Company& operator+=(Company& company, const std::shared_ptr<Employee>& employee);

  private:
    std::string m_name;
    std::vector<std::shared_ptr<Employee>> m_employees;
};

Company& operator+=(Company& company, const std::shared_ptr<Employee>& employee);
