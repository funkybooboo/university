# Defaults
The compiler will provide default implementations for a select few methods/operators, if no implementation is provided.

### Constructor
A default constructor is provided that takes no arguments. The behavior of this default is 
["the same effect as a user-defined constructor with empty body and empty initializer list.
That is, it calls the default constructors of the bases and of the non-static members of this class."](https://en.cppreference.com/w/cpp/language/default_constructor)

### Copy
"If a copy constructor is not provided by the programmer, the C++ compiler provides a default member-by-member copy.
The member-by-member copy is exactly the same as what happens with the default assignment operator the compiler
provides (if one is not provided by the programmer)"

### Move
Default constructor and operator provide a member-by-member move similar to copy.

### Destructor
It seems the default destructor will call the destructor of each member in reverse order of their declaration.

# Deleting Defaults
Defaults can be explicitly accepted or deleted with `default` or `delete`.

```c++
class CantCopyMe
{
    public:
        CantCopyMe() = default;
        CantCopyMe(int value) :
        data(value) {}
        CantCopyMe(const CantCopyMe&) = delete;
        CantCopyMe& operator=(const CantCopyMe&) = delete;
    private:
        int data = 0;
};
```
