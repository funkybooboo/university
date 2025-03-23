# Introduction to Enumerations in C++

## What are Enumerations?
- **Enumerations (enums)** allow you to define a set of named integral values.
- They improve code readability and help prevent errors by providing meaningful names.

### Types of Enumerations
1. **Unscoped Enumeration** (Legacy)
2. **Scoped Enumeration** (Recommended)

## 1. Unscoped Enumeration (Legacy)

### Definition
Unscoped enumerations define names for a set of values but can lead to naming conflicts.

### Example
```cpp
#include <iostream>

enum City {
    NewYork,
    Chicago,
    Denver,
    LosAngeles
};

int main() {
    City thisCity = Chicago; // Assign a city
    switch (thisCity) {
        case NewYork: 
            std::cout << "New York" << std::endl; 
            break;
        case Chicago: 
            std::cout << "Chicago" << std::endl; 
            break;
        case Denver: 
            std::cout << "Denver" << std::endl; 
            break;
        case LosAngeles: 
            std::cout << "Los Angeles" << std::endl; 
            break;
    }
    return 0;
}
```

### Key Points
- **Direct Use**: No need to scope the names.
- **Underlying Type**: Integral type.
- **Custom Values**: You can specify values:
  ```cpp
  enum City {
      NewYork = 0,
      Chicago = 1,
      Denver = 2,
      LosAngeles = 3
  };
  ```

## 2. Scoped Enumeration (Recommended)

### Definition
Scoped enumerations provide better type safety and avoid naming conflicts by requiring you to scope the names.

### Example
```cpp
#include <iostream>

enum class State {
    Alabama,
    Arizona,
    Hawaii,
    Kansas
};

int main() {
    State thisState = State::Alabama; // Scoped assignment
    switch (thisState) {
        case State::Alabama: 
            std::cout << "Alabama" << std::endl; 
            break;
        case State::Arizona: 
            std::cout << "Arizona" << std::endl; 
            break;
        case State::Hawaii: 
            std::cout << "Hawaii" << std::endl; 
            break;
        case State::Kansas: 
            std::cout << "Kansas" << std::endl; 
            break;
    }
    return 0;
}
```

### Key Points
- **Scoped Use**: Names must be prefixed (e.g., `State::Alabama`).
- **Underlying Type**: Still integral.
- **Custom Type and Values**: Specify the underlying type:
  ```cpp
  enum class State : unsigned char {
      Alabama = 0,
      Arizona = 1,
      Hawaii = 2,
      Kansas = 3
  };
  ```
