# Introduction to Namespaces in C++

## Motivation
When developing C++ applications, you might encounter naming conflicts, especially with functions that share the same name. 
For instance, how can we differentiate between our custom `sin` function and the `sin` function in the standard library?  
**Namespaces** provide a powerful solution to this problem.

## What are Namespaces?
Namespaces are a feature in C++ that helps organize code by grouping related identifiers and preventing naming conflicts. 
They allow you to:
- **Organize code logically**: Group related functions and classes.
- **Prevent naming conflicts**: Avoid issues with identifiers having the same name.
  
### Comparison with Java Packages
- Namespaces in C++ are somewhat similar to Java packages, but they differ in significant ways:
  - **Namespaces do not imply directory structures.** They simply group identifiers.

### C++20 Modules
- As a side note, C++20 introduces modules, which provide an alternative way to organize and manage code.

## Defining Namespaces

### Example: Wrapping Code
You can wrap your function declarations and implementations within a namespace. Here’s an example using a custom math namespace:

### Header Code
```cpp
namespace mymath {
    double sin(double angle);
    void swap(int& x, int& y);
}
```

### Implementation Code
```cpp
namespace mymath {
    double sin(double angle) {
        return (4 * angle * (180 - angle)) / (40500 - angle * (180 - angle));
    }
    
    void swap(int& x, int& y) {
        int temp = x;
        x = y;
        y = temp;
    }
}
```

## Using Namespaces
To simplify code and avoid repeated namespace prefixes, you can use the `using` directive:

### Using the `using` Directive
```cpp
using namespace std;
```

### Important Notes:
- **Avoid using this in header files**: It can lead to naming conflicts.
- **Caution at file scope**: It’s generally discouraged at file scope.
- **Use it at function scope**: It can be convenient and safer when limited to a specific function.

## Features of Namespaces
Namespaces come with several features that enhance their usability:
- **Nested Namespaces**: You can nest namespaces within each other.
- **Shared Namespaces Across Files**: The same namespace can be used in multiple source files.
- **Multiple Namespaces in One File**: You can define several namespaces within a single file.
- **Anonymous Namespaces**: You can create an anonymous namespace by omitting the namespace identifier, which restricts visibility to the file.

## Nested Namespace Example
Here’s an example of how to define and call functions within nested namespaces:

### Defining Nested Namespaces
```cpp
namespace outer {
    namespace inner {
        namespace goodstuff {
            std::array<int, 4> getPrimes() {
                return { 2, 3, 5, 7 };
            }
        }
    }
}
```

### Calling Functions from Nested Namespaces
You can call the `getPrimes` function in two ways:

#### Direct Call
```cpp
auto primes = outer::inner::goodstuff::getPrimes();
```

#### Using a Namespace Alias
Creating a namespace alias can simplify calls:
```cpp
namespace gs = outer::inner::goodstuff;
auto primes = gs::getPrimes();
```
