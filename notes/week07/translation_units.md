# Compiler Directives
These are instructions given to the pre-processor. The pre-processor does many things, largely a lot of copying and
pasting text. Compiler directives are things like `#include`, `#define`, `#pragma`. When the pre-processor sees an
include statement, it literally copies the contents of the referenced file into the include statement's place.

# Translation Units
When the pre-processor is done with a `.cpp` file, the resulting code is called a translation unit, which the actual
compiler then uses. This is still c++ code, just with all necessary type information and other compiler directives
complete. One translation unit per file.

These are the reason we have header files and source files. A header file can be included in many cpp files, but because
it is just type information that is ok. If it were full of actual logic, the compiler would end up re-compiling the same
stuff many times. Header information will end up in multiple translation units, but implementation will appear ony once.

# Compilation
Translation units are compiled into "object code" which is machine code. The final step is linking.

# Linker
The linker combines the various pieces of object code to create a coherent executable. At this point, all the
implementations have been integrated and you have your final product (exe, dll, so, etc)

# Separate Compilation
This just refers to the idea that a translation unit need only know about type information (ie headers), it does not
need to have implementation until after the linker is done with it.

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

# Vocab
- Compiler Directive: Statements like `#include` that the pre-processor handles before compilation.
- Translatoin Unit: C++ code after the pre-processor is done with it.
- Linker: Combines compiled object code together into a coherent executable.
- Header file: Doesn't include implementation, just type information (.hpp)
- Source file: Does the actual implementation (.cpp)
