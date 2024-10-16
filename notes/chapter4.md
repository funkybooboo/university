## Chapter 4

### Process vs. Thread (4.1)
# 4.1

## Section 2.9 - Booting an OS – System Boot (Section 2.9.2)

The boot process is the sequence of operations that the system performs to load the OS and initialize hardware which makes the system functional. It involves different firmware types and mechanisms depending on the hardware and firmware capabilities.

### Basic Input/Output System (BIOS)

- **Definition**: Software stored on a non-volatile memory chip, such as flash memory or EEPROM (Electrically Erasable Programmable Read-Only Memory), located on the motherboard. It’s considered the “traditional” firmware that starts up when the computer is powered on.
- **Role**: BIOS is responsible for initializing and testing hardware components (like the CPU, memory, and storage devices) and loading the bootloader from the Master Boot Record (MBR) into memory.

#### Steps Involved:
1. **System Tests**: BIOS performs the Power-On Self Test (POST) to check if hardware components are functioning correctly.
2. **Loading the Bootloader**: If tests pass, the BIOS locates the bootloader in the MBR of the hard disk and loads it into memory. The MBR is a small, specific section at the beginning of a storage device that contains information about disk partitions and the boot code.
3. **Loading the OS**: The bootloader takes over and loads the operating system into memory, allowing it to initialize and take control of the system.

#### Limitations:
- Operates in 16-bit mode, limiting its capabilities and speed.
- Has a text-based interface, lacking graphical capabilities and mouse support.
- MBR partitioning has a size limit of up to 2.1 TB for drives.
- Can only initialize one hardware device at a time, making it slower than modern alternatives.
- The process is generally multi-step and slow due to these limitations.

#### Security Issues:
- **Rootkits** (type of malware) can infect the MBR. Since BIOS loads the bootloader from the MBR, a rootkit can execute before the OS starts, allowing it to hide itself from security software.

### UEFI (Unified Extensible Firmware Interface)

- **Definition**: UEFI is the modern replacement for BIOS and is now standard for boot startup. It offers more features and capabilities, enhancing the boot process and system security. It's also pronounced in a non-intuitive way.

#### Features:
- Operates in **32-bit or 64-bit mode**, allowing it to access and manage more memory space efficiently.
- Can provide a **GUI**, enabling user interaction with a mouse, improving usability compared to BIOS.
- Capable of **initializing multiple hardware devices simultaneously**, speeding up the boot process.
- Has **networking capabilities** for remote troubleshooting, helpful for managing systems that have issues during startup.
- Supports larger drives (>2.1 TB) for booting due to its use of the **GUID Partition Table (GPT)** instead of MBR.
- Can **check the OS for validity** before loading, enhancing security by verifying the integrity of the operating system.
- Essentially acts as a mini operating system with its ability to handle various hardware and software tasks independently.

#### GPT (GUID Partition Table):
- **GUID** - Globally Unique Identification ensures that every partition has a unique identifier.
- Unlike MBR, GPT stores boot code in multiple partitions across the drive, improving reliability and recovery options in case of data corruption.
- If one partition is damaged, others can be used to recover.

## Chapter 3 - Process Concepts

### Section 3.1 - Process Concept

- **A Process**: The entities that run on the CPU. Early computer systems (batch systems) referred to these as jobs, and time-shared systems called them user programs or tasks. Modern systems use the term process to describe these active entities, encompassing both user and kernel-level programs. Though the term job may appear in the context of job scheduling, it essentially refers to processes.

### What is a Process?
- A process is essentially a program in execution. While a program is **PASSIVE** (stored on disk), a process is **ACTIVE** (running in memory).
- The process can be in various states but is always considered ready to execute when it’s in memory.

### Process Organization in Memory:
Processes are structured into different sections in memory:
- **Text Section**: Contains the executable code of the program.
- **Data Section**: Stores global variables and resources that the process needs.
- **Heap**: Dynamically allocated memory for objects or data created during runtime.
- **Stack Section**: Temporary storage for activation records, such as function call data (local variables and return addresses).

#### Dynamic vs. Static Sections:
- The **text** and **data** sections have fixed sizes, meaning they don’t change during the process’s lifetime.
- The **heap** and **stack** sections can grow or shrink dynamically. The operating system manages these regions to ensure they don’t collide with each other.

> **Extra Note (Not in Book)**:  
> When a process is created, a kernel stack is also allocated. This stack is specifically used for kernel-mode operations, such as handling system calls. It is stored in a protected memory area, separate from user processes, to maintain system security and stability.

### Process States:
A process can exist in various states during its lifecycle. States may vary slightly between operating systems, but the general states include:
- **New**: The process is being created.
- **Ready**: The process is waiting to be assigned to the CPU.
- **Running**: The process is currently executing on the CPU.
- **Waiting**: The process is paused, waiting for some event (e.g., I/O completion).
- **Terminated**: The process has completed execution and is no longer active.

### Process Control Block (PCB)
The PCB is a critical data structure used by the OS to manage processes. It holds all the information needed to track, control, and manage a process's execution.

#### Categories of Information Stored in a PCB:
- **Process State**: The current state of the process (e.g., ready, running, waiting).
- **Process Counter**: The address of the next instruction the CPU will execute.
- **CPU Registers**: Current values of the CPU registers to be restored when the process resumes execution.
- **CPU Scheduling Information**: Includes priority levels and other scheduling details (discussed further in later chapters).
- **Memory Management Information**: Details like memory limits and allocated segments (more in later chapters).
- **Accounting Information**: Tracks resources used, such as CPU time and process ID.
- **I/O Status Information**: Information on I/O devices assigned to the process and open files.

#### Summary:
The PCB serves as a comprehensive record that contains all the necessary data to manage and resume a process’s execution efficiently.

> **Additional Note (Not in Book)**:  
> The PCB is typically stored in the lower address portion of the kernel stack, a secure memory area inaccessible to user processes, ensuring its integrity and protection.

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
