## Chapter 4

### Process vs. Thread (4.1)
# Section 4.1 - Overview

## Process vs. Thread

- **Process**: A running instance of a program. It contains its own memory space, resources, and state, and the operating system manages multiple processes concurrently to utilize the CPU and other resources efficiently.
  - A computer manages and executes many processes simultaneously, even if it has a single CPU, by rapidly switching between them (context switching).

- **Thread**: The smallest unit of execution within a process.
  - A process can contain one or multiple threads.
  - Each thread within a process shares the same code, data, and resources but operates independently.

> The book refers to threads using the term "task"—treating them in a manner similar to processes since they can be executed independently.

### Shared/Unique Elements of Threads

- **Shared**:
  - **Code**: All threads in a process share the same code base, meaning they run the same program logic.
  - **Data**: Global variables and dynamically allocated memory (heap) are accessible by all threads within the process.
  - **Files and Resources**: Any files or system resources opened by one thread are accessible by others in the same process.

- **Unique**:
  - **Stack**: Each thread has its own stack, which stores information like function calls, local variables, and return addresses. This isolation prevents one thread’s local data from interfering with another’s.
  - **Registers**: Threads maintain their own set of registers, such as the instruction pointer and stack pointer, ensuring that each thread has its independent state of execution.

### Visualizing Threads

- In a **single-threaded process**: Only one thread exists, and the process operates sequentially (handling one task at a time).
- In a **multithreaded process**: Multiple threads exist, each capable of performing different parts of the program concurrently, improving efficiency and responsiveness.

### Managing Threads vs. Processes

- Managing threads and processes are similar, but threads **share** many resources within a process, while processes are **isolated** from each other.

### Thread Control Block (TCB)

- **TCB**: A data structure that contains all the information unique to an individual thread.
- The TCB typically includes:
  - A reference to the Process Control Block (PCB) of its parent process to access shared resources like code and files.
  - Pointers to the parent thread and any child threads it creates, allowing the operating system to manage their relationships and dependencies.

### Examples of Multithreaded Programs

- **Photo Album Application**: Uses separate threads to generate image thumbnails while keeping the main interface responsive.
- **Web Browser**: Uses threads to process images, handle user input, and download web page data concurrently.
- **Word Processor**: Uses threads for different tasks such as spell-check, auto-saving, and responding to user keystrokes simultaneously; ensures a smooth user experience.

### Motivation for Multithreading

- Single applications often need to perform multiple tasks at once.
  - For example, a media player must read files, decode data, and play audio/video concurrently.
- Creating a process is resource-intensive; it involves allocating memory, setting up resources, and creating a separate execution environment.
- Creating threads is less expensive since they share the process’s resources and memory space.

### Benefits of Multithreading

- **Responsiveness**:
  - Multithreading allows different parts of an application to run concurrently—beneficial for applications with user interfaces, enabling the UI to remain responsive even when other tasks are blocked or taking time to complete (e.g., loading data or performing computations).

- **Resource Sharing**:
  - Since threads share the same address space, they can directly communicate and access shared resources without needing Interprocess Communication (IPC) mechanisms, which are necessary for processes.

- **Economy**:
  - Thread creation has much less overhead compared to process creation. The operating system doesn’t need to allocate a separate memory space or resources as it does for a new process.
  - Switching between threads is more efficient than context switching between processes; threads share the same memory space and only need to save/restore a smaller set of state information.

- **Scalability**:
  - Multithreaded applications can better utilize multi-core processors by distributing threads across multiple cores.
  - This allows a single process to execute multiple threads simultaneously, increasing performance and throughput.

### Multicore Programming (4.2)

### Multithreading Models (4.3)
### Threading Libraries (4.4)
- **User Space Library**
    - No kernel support.
    - Not recognized by the OS.
    - All code and data structures for the library are in user space.
    - This gives a user level library that allows programs to use and create them.
    - **Managed By**: User programs
    - **Context Switching**: Easy, because there is no support of the OS
    - **Multithreading**: Not compatible.
    - **Implementation**: Easy, takes less time.
    - **Blocking Operation**: If any thread is in blocking state all the remaining be blocked.
- **Kernel Level Library**
    - Supported by the OS.
    - Code and data structures in kernel space.
    - This allows the kernel to be multithreaded.
    - Recognized by the OS.
    - **Managed By**: OS
    - **Context Switching**: Difficult, must use kernel system calls to do it.
    - **Multithreading**: Compatible
    - **Implementation**: Difficult in comparison to user threads.
    - **Blocking Operation**: If any thread is in blocking state then other threads will not be blocked.
        - If one thread is blocked it will start to execute a different kernel level thread.
- **Threading Strategies**
    - **Asynchronous**: Parent creates child thread and resumes execution.
    - **Synchronous**: Parent creates children threads and waits for all to complete execution.
        - Typically, used when threads are computing data the parent needs.
- **PThreads**
    - POSIX standard
    - A standard refers to a specification, not an implementation
    - API specifies the behavior of the library, implementation is up to the library vendor.
    - Can be user or kernel threads.
    - **USERS**: macOS, Linux, *NIX
    - Implemented in C++.
- **Windows Threads**
    - Kernel threads
    - WaitForMultipleObjects() takes care of waiting for some number of threads
    - No need to wait for each one in a specific order it is done in a loop
- **Java Threads**
    - Threads are managed by the JVM
    - Typical implementation is the threading model of the underlying macOS
    - **Implementations**:
        - Extend the Thread class and override run()
        - Implement the Runnable interface to define run()
            - Create an instance and pass to Thread constructor

- Videos
    [Kernel Vs User Level Threads](https://www.youtube.com/watch?v=ncnoPkxogIk)

### Implicit Threading (4.5)
- Applications can use hundreds or thousands of threads.
- The more threads the more difficult to manage.
- Pushes the creation and management to run-time libraries and compilers
    - Programmer no longer has to manage creation and destruction.
- **Types**:
    - **Thread Pools**
        - Creating a thread is better than creating a process but still has some overhead.
        - Thread pools create threads ahead of time at start up.
        - The threads sit and wait for work.
        - Submit request to thread pool.
        - If thread is available it is awakened and assigned the task.
        - If no thread is available, the task waits in a queue.
        - **Benefits**:
            - Faster to use existing thread than creating a new thread from scratch
            - Number of kernel thread can be controlled
            - Separates task to be performed from mechanics of thread management.
        - **Used By**:
            - **Windows**: Native OS thread pools
            - **JVM**: Native JVM thread pools
    - **Fork Join**
        - Called fork but really relates to threads, which ususally uses a create.
        - It can also apply to processes too.
        - Join is a threading term. Process term would be wait.
        - Can be called a create/join
        - Synchronous model where threads are created then results are waited on then combined for a solution
        - Split up the work, then combine to get final answer
        - Similar to a divide and conquer algorithm or process fork and wait.
        - Keep breaking up a task into smaller and smaller pieces until a small enough task is created.
        ```code
            Task(Problem)
                if problem is small enough
                    solve problem
                else
                    subtask1 = fork(new Task(subset of problem))
                    subtask2 = fork(new Task(subset of problem))

                    result1 = join(subtask1)
                    result2 = join(subtask2)

                    return result1 + result2
        ```
        - **Java Implementation**:
            - Use RecursiveTask class
            - It will create and manage the pool of threads
            - It is threads forking threads.
    - **OpenMP**
        - Set of compiler directives and an API in C, C++, FORTRAN 
        - Compiler directives to instruct the compiler to parallelize a block of code 
        - A Compiler directive is a tag in code 
        - Works in a shared memory environment 
        - As many threads as their are processors will be created 
        - Notice 
            - No explicit creation of threads 
            - No explicit joining of threads 
            - The compiler handles it all 
    - **Grand Central Dispatch**
        - Developed by Apple for MacOS and iOS 
        - Combination of 
            - Runtime library 
            - API in C, C++, Objective-C, and Swift languages 
        - Language extensions (very similar to compiler directives 
        - Similar to OpenMP where a developer identifies parallel sections 
            - ^{ parallelFunction(data) } 
            - Caret followed by enclosing braces 
        - **Types of Queues**:
            - Serial 
                - Blocks removed in FIFO order 
                - Only one task executing at a time to ensure sequence 
                - Can have multiple serial queues that run in parallel to each other 
            - Concurrent 
                - Blocks removed in FIFO order 
                - Multiple tasks can be removed at once and executed in parallel 

    - **Intel Thread Building Blocks**
        - C++ library 
        - No compiler or language support 
        - Syntax 
            - parallel_for (beg, end, body) 
            - Iterates from beg to end -1 
            - size_t is a data type. Often an unsigned long 
            - ending value is n-1 
            - 3rd parameter is a lambda function (anonymous function) 
            - Similar to Fork-Join where the amount of threading is determined by the library 

### Threading Issues (4.6)
- A few issues to consider when using threads 
- **Semantics of Fork** 
    - fork() duplicates a process 
    - What about threads? Are they duplicated? 
        - Some systems have two versions of fork 
        - Some only duplicate the thread that called fork 
    - What about exec? It replaces the process, but what about threads? 
        - Usually all threads are replaced as well 
- **Signal Handling** 
    - Used by systems to notify a process of an event 
    - Can be within a program 
        - illegal memory access 
        - divide by zero 
- Can be external 
    - Ctrl-c 
    - Timer 
- **Process** 
    - Signal generated 
    - Signal delivered to process 
    - Signal handled 
        - Default handler (OS) 
        - User-defined (process provided) 
- **Single threaded programs** 
    - Delivery is simple because there is only one thread in the process to handle it 
- **Multi threaded programs** 
    - Which thread should receive the signal? 
        - Deliver to relevant 
        - Deliver to all 
        - Deliver to some subset 
        - Deliver to a thread assigned to receive all signals 
    - Deliver depends on the type of signal 
- **Thread Cancelation** 
    - Terminating a thread before it has finished its work 
    - Thread to be canceled is called a target thread 
    - Types 
        - Asynchronous 
            - Thread terminates immediately 
    - Deferred 
            - Thread checks to see if it should terminate 
            - Signal arrives, but nothing happens until thread checks 
            - Thread can then do "cleanup" 
- **Issues** 
    - What happens to the resources a thread has allocated 
    - What happens if a thread is stuck/frozen but deferred is being used? 
