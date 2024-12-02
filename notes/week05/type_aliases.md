# Introduction to Type Aliases in C++

## What are Type Aliases?
Type aliases allow you to create an alternative name for an existing data type. This can improve code readability and maintainability.

### Two Ways to Create Type Aliases
1. **`typedef` (Legacy)**
2. **`using` (Modern C++)**

### Key Difference
- **Functionality**: There is no functional difference between `typedef` and `using`.
- **Preferred Usage**: `using` is preferred in C++11 and later for consistency with other assignment patterns.

---

## 1. Using `typedef`

### Syntax
```cpp
typedef [type] [alias];
```

### Components
- **`typedef`**: The required keyword.
- **`[type]`**: The existing data type.
- **`[alias]`**: The new name (alias) for the type.

### Example
```cpp
typedef int MyInt;             // Alias for int
typedef std::string MyString;  // Alias for std::string

MyInt a = 10;                  // Using the alias
MyString s = "Hi Mom!";        // Using the alias
```

---

## 2. Using `using`

### Syntax
```cpp
using [alias] = [type];
```

### Components
- **`using`**: The required keyword.
- **`[type]`**: The existing data type.
- **`[alias]`**: The new name (alias) for the type.

### Example
```cpp
using MyInt = int;             // Alias for int
using MyString = std::string;  // Alias for std::string

MyInt a = 10;                  // Using the alias
MyString s = "Hi Mom!";        // Using the alias
```

---

## One More Thing: Using with Templates
The `using` keyword has an additional capability that `typedef` does not—supporting template aliases.

### Example
```cpp
template <typename T>
class Original {}; // A template class

template <typename T>
using Alias = Original<T>; // Alias for Original<T>

// Usage
Alias<int> myObject; // Equivalent to Original<int>
```

### Explanation
- **`Alias<T>`**: This creates a type alias for `Original<T>`, making it easier to work with template classes.
