# Code
For Dean's example code, check [here](../../class/code/Constructors/)

# Constructors
C++ has a concept similar to "Constructor Chaining" but called "Constructor Delegation". This means that by default
constructors are not directly inherited, but you can use them in the derived constructor to do some of the work.
If you don't explicitly use constructor delegation, then default constructors are called from Basest -> Base -> Derivedest.
This means if you want to use parameters, you will have to use constructor delegation.
Example if `Vehicle` is derived from `Entity`:
```c++
Vehicle::Vehicle(Color color, double facing, double posX, double posY) :
    m_color(color),
    Entity(facing, posX, posY)
{
}
```

### Inheriting Constructors

Unlike Java, you can inherit constructors with the `using` keyword. It's not clear why you would do this instead
of just using delegation. `Monster` from the code samples does this, but doesn't do anything different from `Vehicle`,
which does not.
```c++
class Monster : public Entity
{
    public:
        using Entity::Entity; // inheriting the Entity constructors
```

# Destructors
Destructors are meant to clean up resources when an object goes out of scope. This includes responsibilities like
deleting pointers, closing files, or keeping track of instance count.

Destructors are called in reverse order from constructors, that is the derived class goes first, and then each base
class up the hierarchy is destructed. You can override destructors, just make sure to use `virtual`.
