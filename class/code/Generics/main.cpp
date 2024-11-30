#include "MyClass.hpp"
#include "stack.hpp"
#include "utils.hpp"

#include <array>
#include <iostream>
#include <print>
#include <string>
#include <vector>

void demoTemplateStack()
{
    using namespace std::string_literals;

    usu::stack<int> zipCodes(10);
    usu::stack<std::string> cities(10);

    cities.push("Logan"s);
    cities.push("Hyde Park"s);
    cities.push("Smithfield"s);
    cities.push("Amalga"s);
    cities.push("Newton"s);

    while (!cities.isEmpty())
    {
        std::print("{}\n", cities.pop());
    }
}

void demoVariadicTemplates()
{
    //    std::print("The max of 4 and 8 is: {}\n", max(4, 8));
    //    std::print("The max of 4, 8, and 6 is: {}\n", max(4, 8, 6));

    std::print("The max of 2, 3.14159, 1.67f is: {}\n", max(3.14159, 2, 1.67f));

    std::cout << "How many types: " << howManyTypes(1, 2.0, "3", true, false) << std::endl;
    std::cout << "How many types: " << howManyTypes() << std::endl;
}

int main()
{
    using namespace std::string_literals;

    // std::cout << xtoy(2, 8) << std::endl;
    // std::cout << xtoy(2.2, 8) << std::endl;

    // std::vector<int> primes1{ 1, 2, 3, 5 };
    // report(primes1);

    // std::array<int, 4> primes2{ 1, 2, 3, 5 };
    // report(primes2);

    //// int primes3[4]{ 1, 2, 3, 5 };
    //// report(primes3);

    // repeat<std::string, 4>("This message"s);
    // repeat<double, 6>(3.1415926);

    /* MyClass<int> value1(6);
     MyClass<std::string> value2("This is a trival inline example"s);

     std::print("{} : {}\n", value1.getData(), value2.getData());

     MyOtherClass<int> value3(6);
     MyOtherClass<std::string> value4("This is a separate decl and defn example"s);

     std::print("{} : {}\n", value3.getData(), value4.getData());*/

    // demoTemplateStack();

    demoVariadicTemplates();

    return 0;
}
