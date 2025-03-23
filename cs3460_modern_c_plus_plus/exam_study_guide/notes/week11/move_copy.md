# Code
These concepts especially benefit from the complete example [here](../../class/code/Constructors/).

# Copy
Copy is both a type of constructor and an operator. Copy is called when using `=` to set one object from another,
or as a constructor by passing in an existing object. 

In copy, both objects live on past the assignment, so the internals of the methods reflect this. In general, you just
have to copy over any data from the right hand side into the left hand side. When used as an operator, 
the copy method of the left hand side is called and passed the right hand side.

# R-Values
R-values are temporary values that do not live beyond the expression in which they are used.
That is in contrast to l-values, which do live beyond the expression in which they are used.

The idea behind the names is an l-value can appear on the left or right hand side of a statement (such as move/copy 
with an `=`) while r-values are expressions that appear only on the right hand side.

### Example
```c++
int x = 44;
int y = 66;
int z = x * y;
```
In this example, the l-values are x, y, and z. They all live beyond any of these statements. `x * y` is an r-value, it
does not make sense to put this on the left of a statement, nor does it stick around after being calculated (except
through storage in z)

You can obtain a reference to an r-value by using two ampersands: `int&& xyRef = x * y;`. I don't think you normally do
this, but that is the type that gets passed into the move operator/constructor so it's useful there.

# Move
Move is both a type of constructor and an operator. The move operator is called when using `=` to set one object from 
another, where the right hand side is an r-value. The method is called on the left hand side object, and passed an
r-value reference to the right hand side. 

Inside the methods, we swap resources from the rhs to the lhs. This sets up the lhs with the data from rhs, like copy.
By giving the rhs the resources that used to belong to the lhs, we set up those resources to be destructed once the
expression is over and the r-value is destroyed.

# Rule of Five
If any one of the following are provided by the programmer, then all should be provided without relying on defaults.

* Copy Constructor
* Move Constructor
* Assignment Operator
* Move Assignment Operator
* Destructor (depending on the nature of the class)
