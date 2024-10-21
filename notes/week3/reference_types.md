## REFERENCE TYPES

References in C++ have **Nothing** to do with a Java reference. In C++, there is no such thing as an undefined reference since they are always an alias to a pre-existing variable. 

### & Decorator
& - This is the symbol we use to denote a type as a reference.

```c++
double goldenRatio = 1.6180339887;
double& alias = goldenRatio;
```
This symbole goes on the *type declaration* of the variable. 

### Using it in a function
This function swaps x and y and takes in a reference of each variable as part of the parameters.
```c++
void swap(int& x, int& y)
{
    int temp = x;
    x = y;
    y = temp;
}
```

### Incorrect/Illegal Uses

```c++
// Cannot reference an actual int. It MUST be a variable.
swap(a, 6); 

// Due to the use of const, we cannot reference it as that would allow us to modify the variable
const double PI = 3.14159;
double& z = PI;

// While you can do Pass by Reference in C++, you cannot return a reference if the parameters are pass by value
int& doubleValue(int n)
{
    return 2 * n;
}
int& twiceValue(int n)
{
    int twice = 2 * n;
    return twice;
}
```

### An Important Note
There is one other thing in C++ that uses the & symbol as a part of it, and that is the dereference operator.
An important distinction between the decorator and operator is *where* they are placed.
```c++
int& reference_val = 90
... do pointer work ...
auto derefenced_var = &a
```
The Reference decorator is **always** on the type declaration of a variable. Whereas the operator will **always** be put in front of the variable that wants to be dereferenced.