## CONDITIONALS

### Basics
Mostly the same as Java, but there are some additional capabilities that we'll discuss. It has the usual types we've seen before: 
- if
- if else
- twitch
- Ternary operator (x ? y : z)

C++ will allow either boolean or numeric values, in contrast to Java only evaluating to a boolean.
If you choose to do numeric values in C++, any non-zero option is considered true, otherwise it is false.

### Initializers
Both *if* and *switch* statements allow for an initializer statement.

**If Statements**
The second part works very similarly to Java. You can do >/=/< types of checks and others with it. 
```c++
// The first part of the if statement is an initializer. The scope is only for the if statement.
if (std::string message = getMessage(); message.size() > 0)
{
    std::cout << message << std::endl;
}
```

**Switch Statements**
Differences from Java:
- Cannot switch on strings, it must be numerical or some kind of enumeration
- Also allows for an initializer like *if*
- Has the attribute [[fallthrough]]. This just means that if there isn't a break on the case, then it'll continue after the fact.

```c++
// The format is similar to if with the initializer first and the case second.
switch (int input = getUserInput(); input)
{
    case 1:
        std::cout << "1 selected" << std::endl;
        break; // Using a break here will prevent a "fallthrough" occuring.
    case 2:
        std::cout << "2 selected" << std::endl;
        break;
    default:
        std::cout << "something else selected" << std::endl;
}
```

### Comparing Strings
In Java, we're used to using .equals() and .compareTo() for strings since == compares the references rather than the contents of two strings.

In C++, we can use the == operator normal as it does compare the contents.
```c++
std::string message1 = "Hello World!";
std::string message2 = "Hello ";
message2 += "World!";

if (message1 == message2) // Resolves to true
```

