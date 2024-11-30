#include "Company.hpp"

Company::Company(std::string name) :
    m_name(name)
{
}

std::shared_ptr<Employee> Company::findbyName(std::string nameFirst, std::string nameLast)
{
    auto result = std::find_if(
        m_employees.begin(), m_employees.end(),
        [nameFirst, nameLast](std::shared_ptr<Employee> employee)
        {
            return nameFirst == employee->getNameFirst() && nameLast == employee->getNameLast();
        });

    return *result;
}

// Company& Company::operator+=(const std::shared_ptr<Employee>& employee)
//{
//     if (employee != nullptr)
//     {
//         this->m_employees.push_back(employee);
//     }
//
//     return *this;
// }

Company& Company::operator-=(const std::shared_ptr<Employee>& employee)
{
    if (employee == nullptr)
    {
        return *this;
    }
    auto iterator = std::remove_if(
        m_employees.begin(), m_employees.end(),
        [employee](std::shared_ptr<Employee> test)
        {
            return *test == *employee; // See next slide
        });
    m_employees.erase(iterator);
    return *this;
}

Company& operator+=(Company& company, const std::shared_ptr<Employee>& employee)
{
    if (employee != nullptr)
    {
        company.m_employees.push_back(employee);
    }

    return company;
}
