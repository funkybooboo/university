# Operator Overloading
Operator overloading allows your classes to function with operators such `+`, `()`, `[]`, `->`, etc. You will provide a
method that takes the necessary arguments and returns the necessary value to make the operator work. These methods are
named by putting the operator itself after the word "operator", `operator<SYMBOL>()`. If you had a class called `Addable`,
you would implement addition with something like this `Addable operator+(const Addable& other)`.
Inside that method, what it means to add two instances of this class is for you to determine and return the correct result.

The operator parameters and return types are not necessarily special and pre-defined.
Class notes include an example of a `Company` class which allows adding new `Employee` objects by use of the `+=` operator.
Thus, the signature is `Company& operator+=(const Employee& employee)`(simplified from original example for clarity).
One could also add a method `Company& operator+=(const Company& otherCompany)`, which would allow you to merge two companies.
This also means that using a const reference parameters is not required, it is just a good idea in the case of addition
and many others.

Extending this, you can even override the same symbol in different ways. For example, `MyClass& operator*()` was used 
in the shared pointers assignment to dereference the internal pointer. Meanwhile `MyClass operator*(const MyClass& other)`
could be used for multiplication.

# Combinations of operators
It's important to note that each operator must be overloaded individually, for example implementing `<` and `=` will not
automatically give you `<=`. You still have to do `<=` individually.

While not required, many operators make the most sense as a part of a larger package of operators. If you
already have `<`, it probably makes sense to also have `>`, `<=`, `>=`, and `==`. This gives rise to the "Rule of Five",
which is discussed in [Move/Copy notes](../week11/move_copy.md).

# Move/Copy operators
Move and copy have a constructor form and an operator form, so I gave them their own [file](../week11/move_copy.md).

# Common operator overloads
* `+` (addition)
* `-` (subtraction)
* `*` (multiplication OR dereference)
* `/` (division)
* `==` (equality comparison)
* `!=` (inequality comparison)
* `!` (logical NOT)
* `<` (less than)
* `>` (greater than)
* `<=` (less than or equal to)
* `>=` (greater than or equal to)
* `()` (function call operator)
* `[]` (subscript/array index operator)
* `=` (assignment operator)
* `+=` (addition assignment)
* `-=` (subtraction assignment)
* `*=` (multiplication assignment)
* `/=` (division assignment)
* `++` (increment)
* `--` (decrement)
* `->` (member access through pointer)
