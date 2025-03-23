# Quiz 1

## Question 1.1
What is the name of the part of the operating system that is always running?  
**Responses**  
1. CPU - the thing that runs instructions  
2. Kernel - the thing that is always running  
3. Interrupt - signal that gets sent to the CPU, does context switching, runs the handler  
4. Scheduler - also called dispatcher sometimes, there is more than one of them, they schedule all the processes in the queues to be put onto the CPU  
5. Process Manager - Tree structure manager  

## Question 1.2
Which is NOT a basic component of a Computer System?  
**Responses**  
- CPU  
- Device Controller - drive, CD disk, local buffer that shows how it talks with the OS.  
- Network Adapter  
- Shared Memory - RAM  
- Common Bus - everything connects on this  

## Question 1.3
What is the name for the instructions that are executed when a certain event happens such as a keyboard key being pressed?  
**Responses**  
- Kernel  
- Interrupt Service Routine - code  
- Catch and Dispatch - catch is interrupt and dispatch is code  
- Interrupt Vector - address for code  
- Interrupt Signal - index for vector  

## Question 1.4
Memory is considered non-volatile because its data disappears when the power is cut off.  
**Responses**  
- True - volatile means goes away when power goes away  
- False - When something is non-volatile, the data does not disappear  

## Question 1.5
Place the data storage devices in order from fastest to slowest.  
1. Registers  
2. Cache  
3. Main Memory  
4. Hard Disk  
5. Magnetic Tape  

## Question 1.6
Fault tolerance is one of the key advantages of a multi-processor system over a single-processor system.  
**Responses**  
- True - Increased reliability, shares resource, graceful degradation  
- False - if he puts a not  

## Question 1.7
In Symmetric Multiprocessing (SMP), each processor can run any task, including OS (not user-level) processes.  
**Responses**  
- True - Symmetric means all are equal  
- False - Asymmetric has boss-worker relation where one runs user, one runs kernel, and divides tasks  

## Question 1.8
Which type of processor configuration makes a shared cache on the CPU possible?  
**Responses**  
- Single Chip  
- Multi-core - core is the bit, many cores on the same chip  
- Multi-chip - only one chip  
- Symmetric  
- Clustered  

## Question 1.9
What is the main purpose of having DMA?  
**Responses**  
- DMA isn't a thing.  
- Utilize multiple CPUs for efficiency  
- Device drivers are able to use less cache  
- Reduce the burden on the CPU for performing I/O - Allows the devices to write directly without having the CPU do extra work to tell it where to write  
- Enhance security of data while being transferred - anything that accesses the memory without a form of authentication or cipher, etc., is not secure.  

---

# Quiz 2

## Question 1.1
What is the startup process called to get the Operating System running when a computer is turned on?  
**Responses**  
- Interrupt Service Routine  
- Bootstrapping - The startup process that the computer needs to run to know where to start  
- Asymmetric Startup  
- Multiprocessing  
- Multitasking  

## Question 1.2
Which of the following is NOT one of the resource management responsibilities of the OS? Choose "None" if no other answer is applicable.  
**Responses**  
- Process Management  
- Memory Management  
- File System Management  
- Mass Storage Management  
- Cache Management  
- I/O Subsystem Management  
- None of the Above  

## Question 1.3
What security term describes ensuring that data is correct, authentic, and reliable?  
**Responses**  
- Availability - able to do something  
- Confidentiality - Only the person who owns sees it  
- Safeguard  
- Integrity - is there when you need it  
- Protection  

## Question 1.4
Mounting and Unmounting are part of File System Management.  
**Responses**  
- True  
- False - it's part of I/O management  

## Question 1.5
What is the command used in a Linux shell to display the current working directory path?  
**Responses**  
- PATH - contains the paths to search for executables  
- Pwd - print working dir  
- Echo - puts text onto stdout  
- Cd - change dir  
- Ls - list files  

## Question 1.6
Which is NOT one of the three main categories of User Interfaces for an OS?  
**Responses**  
- Voice  
- GUI  
- Command Line  
- Touch  

---

# Quiz 3

## Question 1
Using a register to pass data from a user mode process to the OS running in kernel mode is not possible on modern computer systems.  
**Responses**  
- True  
- False - possible because that's how message passing works  

## Question 2
What OS structure has a lot of communication overhead and therefore is not commonly used, but the model is used in computer networking?  
**Responses**  
- Modular  
- Symmetric High Speed Information Transfer  
- Microkernel - as little as possible in the kernel  
- Monolithic - one block  
- Layered - TCP, binary, IP  

## Question 3
A function call made in a program is always the actual OS system call.  
**Responses**  
- True  
- False - It is usually multiple system calls  

## Question 4
In practice, what is the most commonly used OS structure?  
**Responses**  
- Microkernel  
- Hybrid - good parts of everything  
- Monolithic  
- Modular  
- Layered  

---

# Quiz 4

## Question 1
What terms describe the process of changing the process that is running on the CPU?  
**Responses**  
- Context Switch - Changing the process the CPU is currently running is the definition  
- Overhead - extra work you have to do for an operation  
- Kernel Change  
- Instruction State Record - state info for scheduler  
- Queue Swap - scheduler moves processes around  

## Question 2
What is the name of the state where a process must be before moving to the running state?  
**Responses**  
- New  
- Queued  
- Ready - Must be ready before it can be run  
- Waiting  
- Scheduled  

## Question 3
A process in memory has a number of different sections. Which sections of the process are dynamic in size? Mark all that apply. You must get all correct to receive full credit.  
**Responses**  
- Stack - call stack  
- Data - Static constants  
- No other answers are correct  
- Text - the code in binary format  
- Heap - where objects go  

## Question 4
A process is a program that is in memory and is executing. A Process Control Block is another name for the location of the program in memory.  
**Responses**  
- True  
- False - A process control block is the data structure that holds the context the executing process needs to run  

## Question 5
The terms program and process mean the same thing.  
**Responses**  
- True - He is being pedantic here. He sucks ass.  
- False - no  

## Question 6
What is the name of the system call used to create a new child process?  
**Responses**  
- New - not a system call  
- Exec - replaces the process control block of a fork  
- Wait - wait until child gives exit code  
- Split - chunks a file into smaller categories  
- Fork - make copy of parent  

## Question 7
BIOS (Basic Input Output System) is the most common way modern systems execute the system startup process. BIOS can run with a GUI interface and allows for much larger-sized hard drives than earlier system startup processes.  
**Responses**  
- True  
- False - UEFI is more common now because of bigger table size  

## Question 8
What does UEFI use that is synonymous with the MBR in BIOS?  
**Responses**  
- EEPROM - Electrically Erasable Programmable Read-Only Memory  
- Rootkit - virus  
- Overhead - more boilerplate  
- Firmware Interface - OS uses this  
- GUID Partition Table - similar to the MBR but more capable  

## Question 9
The Process Control Block contains information to enable a process to be loaded back to the CPU to begin executing where it left off.  
**Responses**  
- True - It has to contain the previous register values to continue where it left off  
- False  

## Question 10
Changing processes on the CPU requires overhead CPU cycles to manage the change. What best describes the number of cycles it takes to perform this change?  
**Responses**  
- 10's of cycles  
- 100's of cycles  
- 1,000's of cycles - THIS ONE!! REMEMBER THIS ONE!!!!  
- 10,000's of cycles  

---

# Quiz 5

## Question 1
Which is NOT one of the main benefits of multi-threaded programs?  
**Responses**  
- Scalability  
- Economy  
- Parallelism - THIS ONE, not guaranteed that the hardware supports  
- Responsiveness  
- Resource Sharing  

## Question 2
Data Parallelism focuses on distributing subsets of data across multiple cores and performing unique operations on each core.  
**Responses**  
- True  
- False - not unique  

## Question 3
______________________ estimates the maximum speedup a program can achieve based on the percentage of a program that must be serial.  
**Responses**  
- Principle of Power  
- Murphy's Law - Interstellar shit  
- Law of Diminishing Marginal Returns - Business stuff  
- Moore's Law  
- Amdahl's Law - THIS ONE  

## Question 4
Ordinary Pipes are unidirectional.  
**Responses**  
- True  
- False - They are bidirectional  

## Question 5
In what type of communication is it never necessary for processes to know about other processes?  
**Responses**  
- Rendezvous  
- Direct Communication  
- Indirect Communication - observer pattern  
- Non-Blocking Send  
- Blocking Send  

## Question 6
The Consumer Producer model is outdated because in today's computing almost all systems are both producers and consumers.  
**Responses**  
- True  
- False - Commonly used 🤓  

## Question 7
Threads share process instructions, data, files, and register values, but each thread has its own stack.  
**Responses**  
- True  
- False - they have their own files and registers  

## Question 8
Remote Method Invocation (RMI) is an example of what?  
**Responses**  
- Shared Memory  
- Bounded Buffer  
- Message Passing - processes pass messages  
- Non-Buffered  
- Pipe  

## Question 9
In a Bounded Buffer model that is implemented as a circular queue, what is an item that is NOT necessary to keep track of?  
**Responses**  
- Write Location - NEEDS THIS, To know where the next item produced should be inserted.  
- Read Location - NEEDS THIS, To know where the next item to be consumed is located.  
- Number of Producers - not this  
- Buffer Size - NEEDS THIS, To manage the capacity and determine if the buffer is full or empty  

## Question 10
In the Shared Memory model, the shared memory must be located within the kernel memory space.  
**Responses**  
- True  
- False - THIS ONE  

## Question 11
In what case does the Sender always block on the receiver?  
**Responses**  
- Empty Bounded Capacity Buffer  
- Unbounded Capacity Buffer - ArrayList capacity only limited by RAM  
- Zero Capacity Buffer - THIS ONE, no capacity  
- Bounded Capacity Buffer  

---

# Quiz 6

## Question 1
Which Threading approach is similar to a divide and conquer recursion solution?  
**Responses**  
- Google Thread Building Blocks - Intel threading  
- Thread Pool - Makes threads beforehand, reuses them to avoid overhead  
- Grand Central Dispatch - Apple thing  
- OpenMP - C/C++, compiler handled  
- Fork Join - THIS ONE, fork and wait but for threads  

## Question 2
In the ___________________________ thread model, a system call made by one thread will never block another thread.  
**Responses**  
- Many-to-One  
- Many-to-Many - 4 user to 2 kernel, Kernel threads must be less than or equal to user threads  
- One-to-One - This is because every thread is connected to one kernel thread  
- Blocker Preventing  
- None of the Above  

## Question 3
Which Threading approach is used by the Java Executor service?  
**Responses**  
- Fork Join  
- Grand Central Dispatch - iOS, macOS  
- Thread Pool - this one; because that's how Java does it  
- Google Thread Building Blocks  
- OpenMP  

## Question 4
In an Asynchronous thread execution strategy, which of the following is true?  
**Responses**  
- The parent creates a child thread then waits for a single child to complete  
- Children threads wait for the parent thread to give a signal before starting  
- The parent creates child threads then chooses one child as the favorite  
- Children threads of the main thread create other children threads  
- The parent creates child threads then continues execution - THIS ONE; executing at the same time  

## Question 5
What term describes the case where a process is only released from the CPU when it voluntarily does so?  
**Responses**  
- Preemptive  
- Non-preemptive - THIS ONE  
- Non-volatile  
- Volatile  
- Concurrency  

## Question 6
In practice, the distribution of CPU burst durations is pretty even with the number of short and long CPU bursts being somewhat equal.  
**Responses**  
- True  
- False - THIS ONE  

## Question 7
What is the term describing pushing creation and management of threads to run-time libraries and compilers, instead of requiring a developer to manage it?  
**Responses**  
- Explicit Threading - when it's not used by run-time  
- Implicit Threading - THIS ONE  
- Fork This Threading  
- Auto Threading  
- Compiler Threading  

## Question 8
In the __________________________ thread model, kernel threads are not used at all and only user threads are used.  
**Responses**  
- Many-to-One  
- Many-to-Many  
- One-to-One  
- Singular  
- None of the Above - have to have at least one kernel thread  

## Question 9
Java threads can be created by creating a Runnable object and passing it to a Thread constructor.  
**Responses**  
- True - THIS ONE  
- False  

## Question 10
PThreads are an implementation of a standard design.  
**Responses**  
- True  
- False - THIS ONE  

## Question 11
What best describes a CPU burst?  
**Responses**  
- Total time for CPU and I/O processing  
- Total amount of time a process needs to fully execute a program  
- CPU time equal to the duration of an OS timer  
- The amount of CPU time a process needs before voluntarily stopping - THIS ONE  
- Total time spent in the ready queue  

## Question 12
What term best describes the time between one process finishing/stopping execution and another one starting?  
**Responses**  
- Interrupt Latency  
- Event Latency  
- Dispatch Latency  
- Context Switch - THIS ONE  
- Preemptive  
