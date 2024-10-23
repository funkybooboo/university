# Stack
The stack is provided by the language/compiler. It is a space in memory used for local variables and function
parameters. A new scope, such as a function call, allocates space on the stack (a stack frame) which is deallocated 
when the scope ends. This means anything on the stack is temporary.

# Heap
The heap is larger than the stack, but less structured. You can allocate memory in the heap so that you can maintain
access to it when a scope ends. With memory from the heap you must manually request the allocation, keep track of the
pointer, and release the memory when done.

# Dynamic Memory
Dynamic memory refers to the use of the heap to dynamically allocate and deallocate memory.

## Heap Allocation
You must tell the language when you want to allocate memory from the heap, when you are done with the memory you must
tell the OS to release the memory. If you allocate memory and forget to deallocate it, you will end up with a memory
leak. This is where you have lost track of the address of that memory, and thus are not using it, however you haven't
told the OS you are done with it. The OS will keep it reserved for you for no reason, whatever caused this to happen may
repeat until you start munching up all the memory.

### Syntax
Allocate memory from the heap using the `new` keyword. This will give you back a pointer which you must use to keep
track of your new memory. When you no longer need the memory, tell the OS to deallocate it with `delete`. Best practice
is to also set your pointer to `nullptr`, this way you don't accidentally try to access memory that may have been
recycled.

See [Memory Management Example](#memory-management)

## Raw Pointers
The most low-level way to engage in dynamic memory is with raw pointers. A pointer stores a memory address. This is
necessary when using heap-allocated memory, but possible with memory from the stack as well.

### Syntax
Add `*` to any type to create a pointer of that type. Use `&variable` to obtain the address of a variable (something you
might then store in a pointer). Use `*pointer` to "dereference" a pointer. That is, access the value the pointer points
to.

See [Pointer Syntax Example](#pointer-syntax)

## Arrays and Pointers
The name of a raw array *is* a pointer to the first element. Thus, allocating an array in the heap is done by a pointer
to the type contained in the array. Arithmetic can be performed on pointers. Adding "1" to a pointer will give the next
address in memory, stepping by the size of the type of the pointer. Adding 1 to a pointer to a 32-bit int will give you
an adress 32-bits later. So technically you could use this instead of normal array syntax.

### Syntax
Make sure to include the `[]` when deleting.

See [Arrays Example](#arrays)

## Pointers and Functions
Pointers can be passed as arguments to functions. They are still pass-by-value by default, a copy of the address is
made, but can also be passed by reference.

# Examples
## Memory management:
```cpp
int* a = new int(1);                            //Allocate an integer in the heap
std::cout << "Safe: " <<  *a << std::endl;
delete a;                                       // Tell the OS to release the memory at address a
std::cout << "Unsafe: " <<  *a << std::endl;    // The memory at this address may be recycled
a = nullptr;                                    // This just ensures that if you erroneously try to use this pointer, the program will crash
std::cout << "Seg fault: " <<  *a << std::endl;
```

## Pointer syntax:
```cpp
int a = 5;
int* a_pointer = &a; //Pointer type, accessing the address of a
std::cout << *a_pointer << std::endl; // Dereference the pointer, giving the value stored at a
*a_pointer = 7; // Use dereference operator to modify value at address
std::cout << a << std::endl; // Observe that this has modified the value of a
```

## Arrays:
```cpp
// Heap array
int* primes = new int[4];
primes[0] = 2;
primes[1] = 3;
primes[2] = 5;
primes[3] = 7;
std::cout << *(primes + 0) << std::endl;
std::cout << *(primes + 1) << std::endl;
std::cout << *(primes + 2) << std::endl;
std::cout << *(primes + 3) << std::endl;
delete []primes;
primes = nullptr;

// Stack array
int primes[] = { 2, 3, 5, 7 };
int* pointerToPrimes = primes;
std::cout << primes[0] << " : " << pointerToPrimes[0] << std::endl;
std::cout << primes[1] << " : " << pointerToPrimes[1] << std::endl;
std::cout << primes[2] << " : " << pointerToPrimes[2] << std::endl;
std::cout << primes[3] << " : " << pointerToPrimes[3] << std::endl;
```

# Vocab List
- Stack: Managed space in memory used for temporary values such as local variables and function parameters.
- Stack frame: Portion of the stack allocated and deallocated with the lifespan of a scope.
- Heap: Large block of memory dynamically allocated by the OS, it is accessible via `new` and pointers.
- Memory Leak: When heap allocated memory is no longer in use, but does not get released.
- Dereference: Access the value stored at a memory address (pointer)

