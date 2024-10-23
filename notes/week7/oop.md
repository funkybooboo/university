# Classes
Classes in C++ have some similarity to something like Java, but the syntax is different in some important ways and the
behavior may be different.

As with functions, class declaration and implementation are split between header files and source files. Include method
declarations in header file, but implement them in source file. See [Translation Units](translation_units.md).

See [Class Implementation](#class-implementation)

## Access Specifiers
The access specifiers in C++ are similar to Java but slightly different
- `public`: All code has visibility
- `protected`: Only class and derived classes have visibility
- `private`: Only class has visibility

In C++, the access level is not specifiec on each declaration of variable/method. Instead they are declared in groups
for each access specification. Technically you could have multiple groups for say `public` but you usually just put them
all together.

See [Class Implementation](#class-implementation)

## Direct Initialization / Member Initializer Lists
This block of code is written with the constructor, but executes before the body.
This allows you to initialize members before the constructor executes and with a slightly more concise syntax.

### Syntax
Easiest to look at example. Put a colon after the function signature of the constructor. 
Between this colon and the body goes a comma separated list of statements.
Member name is used as if a function, with value inside the parantheses.

See [Class Implementation](#class-implementation)

## Dynamic Memory and Objects
Objects in C++ may be on the stack or heap. If the internals of the class allocate memory, it may be on both. 
There is a difference between a pointer to an object and an object value. 
When you have an object value, that is you allocate it on the stack and are not using a pointer, you use the normal `.`
syntax to access members. When you are using a pointer, whether the object is heap allocated or not, you use the `->`
operator to access members. You could avoid `->` by dereferencing the pointer first, then using `.` but that is more
tedious.

### Syntax
Use `.` to access members of an object if you have the object value. Use `->` to access members of an object if you 
are using a pointer.

See [Accessing Value Vs Pointer](#accessing-value-vs-pointer)

# Examples
## Class Implementation
```cpp
class Rectangle
{
    public:
        // This is a constructor with direct initialization and no body
        Rectangle(double width, double height) :
            m_width(width),
            m_height(height)
        {
        }

        double getArea() { return m_width * m_height; }
        double getPerimeter() { return m_width * 2 + m_height * 2; }
        double getWidth() { return m_width; }
        double getHeight() { return m_height; }

    private:
        double m_width;
        double m_height;
}; // Don't forget the semicolon!
```

## Accessing Value Vs Pointer
```cpp
//Value
Rectangle r1(2, 4);
std::cout << "(width, height) = (" << r1.getWidth() << ", " << r1.getHeight() << ")" << std::endl;

//Pointer
Rectangle* r2 = new Rectangle(4, 6);
std::cout << "(width, height) = (" << r2->getWidth() << ", " << r2->getHeight() << ")" << std::endl;
delete r2;

// Still a pointer
std::shared_ptr<Rectangle> r3 = std::make_shared<Rectangle>(6, 8);
std::cout << "(width, height) = (" << r3->getWidth() << ", " << r3->getHeight() << ")" << std::endl;
```
