## Chapter 4

### Process vs. Thread (4.1)
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
