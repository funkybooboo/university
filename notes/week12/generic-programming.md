## Overview
C++ uses the term *Templates*. A bit different from Java *Generics*.

### Function Templates
Form
```c++
template <typename type>
[return value] functionName([parameters])
{
// function body
}
```
- template: required keyword
- typename: required keyword
- type: Placeholder for the actual paramterized type.
    - Can also do multiple: template <typename T, typename R>
- [return value] is the return type
- [parameters] are the function's parameters.

**Example**
Template to raise x to the power of y
- x is any numeric type
- y is any integer type

```c++
template <typename T>
T xtoy(T x, unsigned int y)
{
    T result{ 1 };
    for (int i{ 0 }; i < y; i++)
    {
        result *= x;
    }
    return result;
}
```
We can allow x to stay as T since we can keep any kind of number as x, but we need 
y to be an integer so we specify it so there is no misuse of the template.

### Template Instantiation
**Compiler**
When looking at the actual Template code, it will parse and verify syntax, but no object code is generated.
Once it reaches actual usage of the Template, it will instatiate it:    
- Binds the types to the parameters in the Template.
- Generates the C++ code necessary for it to run.
- Will attempt to compile and generate the object code.

### Class Templates
Form
```c++
template <typename type>
class ClassName
{
// class code goes here
};
```
Object declaration looks like this: `Classname<int> myObj;`

**Implementation**
Non-template code can be placed in both .hpp and .cpp files.
Template code itself is only in .hpp files.
- Only exception is template specializations

**Template Styles**
- Inline
    - During the template instantiation, all methods are instantiated, regardless of use.
    - Similarly how we do things in Java.
    - Problems:
        - Increased compilation time
        - Increased link time
        - Larger executable file size, typically.
- Separate declaration & definition
    - More similar to how we've done a lot of C++ code by splitting things between .hpp and .cpp files. This has all the same benefits as everything else when doing this.

### Default Template Type
You can set a default type for the template parameter.

```c++
template <typename type = bool>
class ClassName
{
// class code goes here
};
```

### Variadic Templates/Template Parameter Pack
Used for when you need to accept a variable number of template parameters.
- Accepts avariable number of template parameters.
- Uses template paramter pack
    - Allows you to use zero or more type arguments
    - May be mulitple types. And can use non-template types or other templates as types.

General Form
```c++
template <typename... Ts>
[either a function or class declaration]
```

Pay attention to the '...' following the typename. This right here specifies the use of the parameter pack. Ts is just a convention, however, and can be any identifier.

Unique things about it
- No way to access types. At all.
- Doesn't have a .size() member.

**Example**
Max Template
```c++
template <typename T>
T max(T x)
{
    return x;
}
template <typename T, typename... Ts>
T max(T x, Ts... ts)
{
    return (x > max(ts...)) ? x : max(ts...);
}
```
We've made two max templates now. The first one is essentially a base case when one item is passed in. The second is for two or more. We refer to this as *Pack Expansion*

**Size**
There is a way to find out the number of types in a pack aka the size. We use the sizeof... operator. The elipses are necessary.

```c++
template <typename... Ts>
unsigned int howManyTypes(Ts... ts)
{
    return sizeof...(Ts);
}

std::cout << "How many types: " << howManyTypes(1, 2.0, "3", true, false) << std::endl;
std::cout << "How many types: " << howManyTypes() << std::endl;
```

### Template Specialization
We typically will use templates as a means to provide generalized functions and classes so that any data type can be used. But there are times when the function should do different things for specific types. That is where this comes in. 

The How To:
- Define a template (function or class) as normal
- Declare the specialization in an .hpp file.
- Implement this specialization in a .cpp file.
    - We will keep it separate since it follows the proper methods and is an actual implementation of a specific type.

**Example**
Generic Templates
```c++
template <typename T>
class DataStorage
{
    public:
        DataStorage(std::initializer_list<T> data);
        std::size_t size() { return m_data.size(); }
        T& operator[](const std::size_t index);
    private:
        std::vector<T> m_data;
};
template <typename T>
DataStorage<T>::DataStorage(std::initializer_list<T> data)
{
    for (auto&& item : data)
    {
        m_data.push_back(item);
    }
}
template <typename T>
T& DataStorage<T>::operator[](const std::size_t index)
{
    return m_data[index];
}
```
Some Specializations
```c++
template <>
class DataStorage<char>
{
    public:
        DataStorage(std::initializer_list<char> data);
        std::size_t size() { return m_data.size(); }
        char& operator[](const std::size_t index);
    private:
        std::string m_data;
};

DataStorage<char>::DataStorage(std::initializer_list<char> data)
{
    m_data.resize(data.size());
    for (std::size_t position{ 0 }; auto&& item : data)
    {
        m_data[position++] = item;
    }
}
char& DataStorage<char>::operator[](const std::size_t index)
{
    return m_data[index];
}
```
Usage
```c++
DataStorage<int> ints{ 1, 2, 3, 4, 5, 6, 7, 8 };
for (std::size_t i = 0; i < ints.size(); i++)
{
    std::cout << ints[i] << ", ";
}
std::cout << std::endl;

DataStorage<char> chars{ 'a', 'b', 'c', 'd' };
for (std::size_t i = 0; i < chars.size(); i++)
{
    std::cout << chars[i] << ", ";
}
std::cout << std::endl;
```
