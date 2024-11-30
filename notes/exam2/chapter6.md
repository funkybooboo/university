# Chapter 6

## Purpose
- **Introduce the Critical Section Problem**: Addressing issues related to the execution of concurrent processes that access shared resources.
- **Present Hardware and Software Solutions**: Discussing ways to solve synchronization issues through both hardware and software mechanisms.
- **Examine Classic Process Synchronization Problems**: Exploring common problems that occur when multiple processes or threads access shared resources.
- **Explore Tools for Synchronization**: Overview of the tools and techniques used to solve synchronization challenges.

## Background (6.1)

### Concurrent vs. Parallel Execution
- **Concurrent Execution**: Multiple processes are executed in overlapping time periods, but not necessarily simultaneously.
- **Parallel Execution**: Multiple processes are executed at the same time on different CPUs.

### Context Switching
- A process may be only partially completed when a context switch occurs.
- Context switching can affect the integrity of data that is shared between multiple processes or threads.

### Integrity of Data
- **Integrity**: The assurance that data is correct, consistent, and reliable across different process executions.
- When multiple processes or threads access shared data concurrently, without proper synchronization, it may lead to data corruption or inconsistency.

### Example: Producer-Consumer Problem (Bounded Buffer)
- A common example of shared memory synchronization is the **Producer-Consumer problem** with a bounded buffer.

#### Producer:
```c
while (true) {
    // Produce something
    while (counter == BUFFER_SIZE)
        ; // Do nothing (busy wait)
    
    buffer[in] = nextProduced;  // Store produced item
    in = (in + 1) % BUFFER_SIZE;  // Circular buffer
    counter++;  // Increment buffer item count
}
```

#### Consumer:
```c
while (true) {
    while (counter == 0)
        ; // Do nothing (busy wait)
    
    out = (out + 1) % BUFFER_SIZE;  // Circular buffer
    counter--;  // Decrement buffer item count
    // Do something with consumed item
}
```

### Logical Code vs. Synchronization Problem
- **Individually**, both the producer and consumer code seem logically correct.
- **Together**, a **synchronization problem** arises due to concurrent access to the shared buffer and counter.

### Race Condition Demo (Synchronized)
- **Race Condition**: Occurs when the output of the program depends on the order in which tasks (instructions) are executed. This can lead to inconsistent or incorrect results.

#### Key Issue: Increment and Decrement Operations
- A race condition can occur between the **increment** (`counter++`) and **decrement** (`counter--`) operators, causing data corruption.
- **Adding/Removing Synchronization**: Using the `synchronized` keyword can resolve this problem by ensuring mutual exclusion when accessing shared variables.

### Source Code vs. Machine Code

- **Source Code**: The high-level code (e.g., C or Java) that needs to be compiled or interpreted to machine code for execution.
- **Machine Code**: The low-level assembly instructions executed by the processor.

For example, consider the following operations:

#### `counter++` (Increment):
```assembly
load reg1, counter   // Load counter into reg1
inc reg1             // Increment reg1
load counter, reg1   // Store the updated value back into counter
```

#### `counter--` (Decrement):
```assembly
load reg2, counter   // Load counter into reg2
dec reg2             // Decrement reg2
load counter, reg2   // Store the updated value back into counter
```

Due to **context switching**, the following might occur:
1. `load reg1, counter` → reg1 = 5
2. `inc reg1` → reg1 = 6
3. `load reg2, counter` → reg2 = 5
4. `dec reg2` → reg2 = 4
5. `load counter, reg1` → counter = 6
6. `load counter, reg2` → counter = 4

**Expected Result**: The value should be 5 (one increment and one decrement).
**Actual Result**: Due to race conditions, the final counter value may be incorrect (in this case, 4).

### Race Condition
- A **race condition** occurs when the outcome of a program depends on the timing or order of executed instructions.
- It is important to note that **a race condition is not a bug**—it is the situation where the program's behavior is unpredictable based on the order in which instructions are executed.

#### Key Points:
- **Race Condition Exists**: If the result of a process depends on the order of execution, a race condition exists.
- **The OS Role**: The OS manages the scheduling of processes, and the application cannot predict the order in which instructions are executed. Therefore, the race condition is not anticipated by the application itself.

---

### Summary
- **Critical Section Problem**: Occurs when multiple processes or threads access shared resources concurrently.
- **Synchronization**: Ensuring that shared data is accessed in a controlled manner to prevent race conditions and maintain data integrity.
- **Race Condition**: A situation where the outcome of a program depends on the order in which instructions are executed. It can lead to inconsistent or incorrect results if not properly synchronized.


## The Critical Section Problem (6.2)

A system of n processes/threads use a shared resource.
- Resource can be a file, shared memory, hardware, data.
- Each process has a "critical section" of code where:
  - It may be changing the state of the shared resource.
  - No other process may be in its critical section at the same time.

### Solution
The critical section solution deals with designing a protocol to ensure only one process is in its critical section at any given time.

### Identify
- **Entry Section**: Request to enter critical section.
- **Critical Section**: Code that modifies the shared resource.
- **Exit Section**: Code to exit the critical section.
- **Remainder Section**: Code not related to the critical section.

```c
do {
    entry_section();  // Request to enter critical section
    critical_section();  // Modify shared resource
    exit_section();  // Exit the critical section
    remainder_section();  // Execute non-critical code
} while (true);
```

### Requirements for Solution
1. **Mutual Exclusion**:
    - Only one process can be in its critical section at a time.

2. **Progress**:
    - If no process is in its critical section, and some process wishes to enter, only processes not in their remainder section can participate in deciding which process will enter its critical section next.
    - The selection cannot be postponed indefinitely.

3. **Bounded Waiting**:
    - There is a limit on the number of times other processes are allowed to enter their critical sections after a process has made a request, and before that request is granted.
    - A process cannot be kept waiting indefinitely.

## Kernel-Level Critical Sections

### Fork Example
- When a process calls **`fork()`**, it creates a new process with a new id.
- The **id** is a kernel variable, and it is returned to the parent process.
- If two processes call **`fork()`** simultaneously, they may compete for the kernel variable that stores the next available id.

### Two Approaches for Handling Kernel-Level Critical Sections
1. **Non-preemptive Kernel**:
    - The kernel code runs until it voluntarily yields the CPU.
    - Kernel code cannot have a race condition because a process completes uninterrupted.

2. **Preemptive Kernel**:
    - More responsive OS because no process can run for an arbitrarily long period of time.
    - Potential for race conditions exist.
    - Must have a mechanism to synchronize data.


## Hardware Support (6.4)

We’ll look at several solutions, but they all require the concept of locking:
- Critical sections are protected via locks.

Hardware provides the support necessary for locking:
- Often not directly accessible to developers.
- OS provides tools that then use these hardware solutions.
- These operations can be used directly as synchronization tools or can be used to create more abstract synchronization mechanisms.

### Three Types

#### Memory Barriers
- Aka. Memory Fence.
- **Memory Model** - The guarantees provided to a program by the computer architecture.
  - **Two types**:
    - **Strongly Ordered**: A memory modification on one processor is immediately visible to all other processors.
    - **Weakly Ordered**: A memory modification on one process may not be immediately visible to other processors.
  - The model varies by processor type. This means OS/Application developers cannot make assumptions regarding the memory model.
- Developers can ensure a Strongly Ordered type of result by inserting a "memory barrier" after updating memory.
  - This is a system call/instruction.
  - This will force all loads and stores to complete before any new loads and stores can start.

##### Example
- **Shared data**:
  ```c
  flag = false;
  x = 0;
  ```

- **Thread 1**:
  ```c
  while (!flag);
  print(x);
  ```

- **Thread 2**:
  ```c
  x = 100;
  flag = true;
  ```

- **Expectation**: Thread 1 prints 100.
- **Reality**: Thread 2's instructions may be reversed on the processor because there is no dependency between `x` and `flag`. This may result in Thread 1 printing 0 or even printing `x` before loading the `flag` variable.

##### Solution
- **Thread 1**:
  ```c
  while (!flag);
  memory_barrier(); // Guarantee flag is loaded before the value of x.
  print(x);
  ```

- **Thread 2**:
  ```c
  x = 100;
  memory_barrier(); // Guarantee x changes before flag.
  flag = true;
  ```

- This forces any load and store prior to the memory_barrier() to complete before anything after it.

#### Hardware Instructions

Modern systems provide one of the following hardware instructions:

- **Test and Set**:
    - Test and then modify a word atomically (as if it happened all at once).

- **Compare and Swap**:
    - Swap the contents of two words atomically.

##### Test and Set
We'll look at a general approach, not a specific system's implementation.

**Code**:
```c
boolean test_and_set(boolean* target) {
    boolean rv = *target;
    *target = True;
    return rv;
}
```

**Pseudocode**:
1. Save a value.
2. Set the original variable to `True`.
3. Return the original value.

This all happens atomically, which prevents another process from checking the value somewhere in the middle of this process.

**Usage**:
- `lock` is False initially and is a shared value.

```c
do {
    while(test_and_set(&lock));
    // Critical section
    lock = false;
    // Remainder section
} while(true);
```

**Explanation**:
- Two or more processes will have similar code.
- If one process starts first and does `test_and_set`, no other process can do `test_and_set` concurrently because it is an atomic process.
- The first process will change `lock` to `True` and return `False`.
- This will block all other processes because their `test_and_set` will return `True`, causing a busy wait.
- Eventually, the first process finishes and changes `lock` to `False`.
- This allows another process to call `test_and_set()`, returning `False`, but it will set the lock to `True` in an atomic process.

##### Compare and Swap
**Overview**:
- Atomic operation.
- Allows processes to compete for a lock.

**Code**:
```c
int compare_and_swap(int* value, int expected, int new_value) {
    int temp = *value;
    if (*value == expected) {
        *value = new_value;
    }
    return temp;
}
```

**Pseudocode**:
1. Save a value.
2. Check to see if the value equals an expected value.
3. If it does, swap a new value into the original variable.
4. Return the original value.

This all happens atomically.

**Usage**:
- `lock` is initialized to 0.

```c
do {
    while(compare_and_swap(&lock, 0, 1) != 0);
    // Critical section
    lock = 0;
    // Remainder section
} while(true);
```

**Explanation**:
- Very similar to `test_and_set`.
- The first process will:
    - Change `lock` to 1.
    - Return 0 (this happens atomically).
- This process can then enter its critical section.
- All other processes will return 1 from `compare_and_swap`, resulting in a busy wait.
- The first process changes `lock = 0`.
- Now another process can get a false in its `compare_and_swap`.

**Why an int?**
- A shared resource may be accessed by more than one thread, but there is some maximum limit.

#### Atomic Variables

`Compare_and_swap` is typically used as a building block for constructing synchronization tools, rather than being used directly. One of these tools is the **Atomic Variable**, which is provided by the system but is actually built with `compare_and_swap()` operations.

##### Increment Example
- We know an increment is actually a multi-instruction sequence, which can happen out of order.
- An atomic operation can be built for an `int` variable.

**Code**:
```c
void increment(int* v) {
    int temp;
    do {
        temp = *v;
    } while(temp != compare_and_swap(v, temp, temp + 1));
}
```

**Explanation**:
- Create a temporary variable.
- Set the temporary variable to the original value.
- Keep doing this until `compare_and_swap` atomically changes `v` to `+1`, so `temp != v` can be false.
- It keeps trying until `v == temp`. `v` is the lock value, which must be what is expected in order for the swap to happen.

#### Producer Consumer Issue

- Two consumers waiting on an empty buffer.
- Producer adds something and increments a count to 1.
- Both consumers could see that `count != 0` and exit their while loops.

Next topics address this need for more robust tools.



## Mutex Locks (6.5)

### Intro

- Previous examples are code examples to show what is necessary in hardware for an OS to implement tools for solving the critical section problem.
- Application developers typically only have access to **Atomic Variables**.
- Instead, the OS provides high-level software tools for application developers that are implemented with the hardware solutions.

### Mutual Exclusion Lock

- The simplest of the OS-provided locking systems.
- Process must acquire a **mutex** to enter a critical section.
- Releases the mutex when complete.
- **Acquire** and **Release** are atomic operations.

**Code**:
```c
acquire() {
    while(!available);
    available = false;
}

release() {
    available = true;
}
```

### Spin-lock

- Busy waiting that doesn't place the thread into a waiting state.
- This is a bad approach for a single core since a concurrent thread is doing nothing.
- Ok on multi-processor:
    - The critical section thread can progress on another core.
    - No context switch required if the mutex is only held for a short duration, compared to the context switch time of moving the thread to the wait state, then back later to running, etc.



## Semaphores (6.6)

- More sophisticated than a **Mutex**.
- Allows for more complex synchronization scenarios.

### Parts

- **Semaphore** is an integer variable.
- After initialization, it is only accessed via **wait()** and **signal()** calls.

### Why an integer?

- Same reason as the **compare_and_swap**.
- The bathroom analogy.

### Code

```c
wait(S) {
    while(S <= 0) {
        ; // busy wait
    }
    S--;
}

signal(S) {
    S++;
}
```

- **Atomic operations** of course.
- All modifications to the Semaphore happen inside the **wait()** and **signal()**.

- A semaphore with an initial value of 1 is just like a **Mutex**.
- A semaphore with an initial value > 1 means there are more than 1 of the type of resource available.



## Monitors (6.7)

- **Mutexes** and **Semaphores** have a weakness:
    - They depend on proper usage by the developer.
    - Every process must implement the **acquire/release**, **wait/signal** protocol properly.
    - One process that doesn't implement it properly can mess things up.

- A **Monitor** is a code construct where mutual exclusion is provided by the **data type** (abstract data type or object).
    - The monitor manages the data and functions to provide mutual exclusion.

### Java Synchronized

- We've seen this before for the last assignment. Let's discuss it in more detail.
    - This is actually a **monitor approach**.
    - A monitor is an object, and can actually be any Java object.
    - Look at **Object** documentation: [Java Object Documentation](https://download.java.net/java/early_access/panama/docs/api/java.base/java/lang/Object.html).
    - Every Java object is of type **Object**.

#### Methods

- `notify()`
- `notifyAll()`
- `wait()` (a few versions)

    - These all have to do with monitor behaviors.

- For a single object, only one **synchronized** method can execute at a time.
    - When used with a method, the monitor is **'this'** or the object whose instance method is synchronized.

- You can create a **synchronized block** with `{}`.
    - `synchronized(object) {}`.
    - Typically, the object is **'this'**.
    - Using blocks can give more control since we can just protect part of a method if we want.
    - We can also use multiple monitors if we only want certain sets of blocks to be protected from each other.
        - Example:
            ```java
            private Object monitor1 = new Object();
            private Object monitor2 = new Object();
            ```

### Monitor Code

- Put **synchronized** on all 4 methods.
- Look at the behavior using **htop**. Two CPUs are overburdened.
- Do **synchronized** blocks properly.
- **htop** shows 4 processors doing a lower load (it shows this when done incorrectly).



## Liveness (6.8)

- **Liveness** refers to the circumstances needed for all processes to make progress.
    - **Liveness failure** means the circumstances are not met, and progress is impeded.

### Deadlock

- **Deadlock** occurs when two or more processes are waiting on each other.
    - Neither process can make progress until the other releases a lock.

#### Example:

- **Process 1**:
    ``` 
    wait(S)
    wait(Q)
    ...
    signal(S)
    signal(Q)
    ```
- **Process 2**:
    ``` 
    wait(Q)
    wait(S)
    ...
    signal(Q)
    signal(S)
    ```

- Note: There is a whole chapter on **Deadlock**, but it won't be covered here.
    - This includes many other causes for deadlock.
    - How deadlock possibilities can be detected.
    - How to recover from deadlock.
    - How to avoid deadlock.

### Priority Inversion

- **Priority inversion** occurs when a higher-priority process is waiting on data currently held by a lower-priority process.
    - Two possible cases:
        1. The lower-priority process is not scheduled because the higher-priority process exists.
        2. The lower-priority process is preempted by the higher-priority process.

- This actually happened on the **Mars rover**.
    - [Mars Pathfinder Long Version Paper](https://www.cs.unc.edu/~anderson/teach/comp790/papers/mars_pathfinder_long_version.html)
    - [Rapita Systems Blog on Mars Pathfinder](https://www.rapitasystems.com/blog/what-really-happened-to-the-software-on-the-mars-pathfinder-spacecraft)
