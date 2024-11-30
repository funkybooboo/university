#include "Employee.hpp"

Employee::Employee(std::string nameFirst, std::string nameLast, std::uint8_t yearsOfService) :
    m_nameFirst(nameFirst),
    m_nameLast(nameLast),
    m_yearsOfService(yearsOfService)
{
}

std::string Employee::getFullName()
{
    return m_nameFirst + " " + m_nameLast;
}

// bool Employee::operator==(const Employee& rhs) const
//{
//     return m_nameFirst == rhs.m_nameFirst &&
//            m_nameLast == rhs.m_nameLast;
// }

Employee& Employee::operator=(const Employee& rhs)
{
    m_nameFirst = "Copy of " + rhs.m_nameFirst;
    m_nameLast = "Copy of " + rhs.m_nameLast;
    m_yearsOfService = rhs.m_yearsOfService;

    return *this;
}