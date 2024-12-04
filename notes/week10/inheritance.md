# Inheritance
Inheritance in C++ is a little different than Java, but has many of the same concepts. For starters, C++ uses different
terminology to express the same ideas, see [below](#vocabulary). I'll focus on the differences here.

## Virtual Methods
In C++, methods must be marked `virtual` to be overridden in a child class. This can simply be added to the method
signature. Good practice is to also include the `virtual` keyword in the child class, though that is not required if it
is not overridden again. Note that `virtual` is not required when providing implementation of the method (as in, the
.cpp file).

Example: `virtual std::string getType() = 0`. 

The `= 0` portion here indicates that this class does not have
any implementation of the method, making it a *pure virtual method* (abstract). By having a pure virtual method, the
entire class becomes "abstract," the compiler will not let you directly instantiate an instance of it.

## Access Modifiers
Similar to Java, slight difference because C++ has no concept of a package.

|Access Location   |public |protected |private|
|------------------|-------|----------|-------|
|Within the class  | yes   | yes      | yes   |
|Derived class     | yes   | yes      | no    |
|Use of object     | yes   | no       | no    |

## Derived Class
Defining a derived class is a little bit different, you will also specify a visibility modifier here. Usually it will be
`public`.

The syntax for the derived class is `class <derived_name> : <visibility modifier> <parent_name>` then go on making the class as normal.

#### Inheritance Visibility Modifiers
|Base / Derived     |public      |protected   |private     |
|-------------------|------------|------------|------------|
|public             |public      |protected   |private     |
|protected          |protected   |protected   |private     |
|private            |not visible |not visible |not visible |

Essentially, anything that is visible to the derived class is restrained to being as private or more private than the
visibility modifier given when deriving.

# Polymorphism
Polymorphism works about as you expect, but there are some things to remember. Firstly, when overriding a method from a
base class, it is a good idea to put `override` at the end of the signature: `virtual int getSizeX() const override;`
(this appears in the hpp part, not the cpp part).

You technically can override a method which is not marked as `virtual`, however it is a bad idea. Method called depends
on what type is being used to access the object.

# Constructors
C++ has a concept similar to "Constructor Chaining" but called "Constructor Delegation". This means that by default
constructors are not directly inherited, but you can use them in the derived constructor to do some of the work.
If you don't explicitly use constructor delegation, then default constructors are called from Basest -> Base -> Derivedest.
This means if you want to use parameters, you will have to use constructor delegation.
Example if `Vehicle` is derived from `Entity`:
```
Vehicle::Vehicle(Color color, double facing, double posX, double posY) :
    m_color(color),
    Entity(facing, posX, posY)
{
}
```

### Inheriting Constructors

Unlike Java, you can inherit constructors with the `using` keyword. This is mentioned, but no reason for doing so is
given. `Monster` from the code samples does this, but doesn't do anything different from `Vehicle`, which does not.
I have found no example of why someone would do this.
```
class Monster : public Entity
{
    public:
        using Entity::Entity; // inheriting the Entity constructors
```

# Destructors
Destructors are called in reverse order from constructors, that is the derived class goes first, and then each base
class up the hierarchy is destructed. You can override destructors, just make sure to use `virtual`.


# Vocabulary
Most concepts from Java exist, but have different names.

|Java term                 | C++ term                       |
|--------------------------|--------------------------------|
|super class               | Base/parent class              |
|sub-class                 | Derived/child class            |
|Abstract method           | Pure Virtual Method            |
|Abstract class            | No term, just a class with PVM |
|Constructor Chaining      | Constructor delegation         |

### Additional terms:
- Virtual: method can be overridden in child class.
