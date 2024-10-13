## Chapter 1

### What are Operating Systems (1.1)

- An operating system is a **program** that sits between the user and the computer hardware.
- It manages computer hardware to run user programs.
- It allows for the use of computer hardware in an easy and convenient way.
- **OS Jobs**
    - **Allocate Resources**
        - Manages all resources.
        - Manage conflicting requests for efficient and fair use.
        - Schedules tasks to run on the CPU and manages hardware interrupts
        - One of the goals of an OS is to use hardware **efficiently**. 
    - **Control Program**
        - Controls the execution of programs to prevent errors and improper use.
        - Runs and executes those programs and can **interrupt** them when necessary.
- The one program that runs all the time is the **kernel**
- The OS can be considered everything that the vendor of that specific OS ships.

### Computer System Organization (1.2.1)
- **Basic components**
    - CPU
    - Devices with controllers
        - Driver for every controller.
        - Device Driver: Provides an interface that the OS can communicate with.
    - Shared Memory
    - Common BUS: The circuitry that carries signals between components
- **Capabilities**
    - Allows for parallel execution of CPU and device controllers.
    - Device controllers all have local buffers that can store small amounts of data.
    - CPU moves data to/from device controllers.
    - Device controllers can send **interrupts** to the CPU.

### Storage Structures (1.2.2)
- **Main Memory**: The only large storage the CPU can access directly, typically volatile or will lose its data when power is lost.
    - Random Access: typically Dynamic Random Access Memory (DRAM)
        - Can access any location at any time. Doesn't need to read linearly to get to an address.
- **Secondary Storage**: Much larger chunk of memory, non-volatile, much slower.
    - Mechanical: Hard disk drives (HDD) with platters.
        - The disk controller manages the interface between the device and the OS.
    - Electrical: Solid state drives (SSD). Much faster than HDD.
    - Both examples of (NVM) non-volatile memory.
- **Storage Hierarchy**: Organized by speed, cost, and volatility (Fastest on top)
```text
              ┌     Registers
Primary   ->  │     Cache
              └     Main Memory                  ↑ Volatile

---------------------------------------------------------------

Secondary ->  ┌     Non-volatile Memory          ↓ Non-volatile
              └     Hard-disk drives

Tertiary  ->  ┌     Optical disk
              └     Magnegtic tape
```
### Interrupts (1.2.3)
    - **Interrupts**: A special signal that gets sent that interrupts the program that is running and the program exeecution jumps to a special routine called an **Interrupt Service Routine**. This allows the OS to react and handle it appropriately
    - **The problem**
        - The CPU can do only one thing at a time and all the hardware it is managing is operating asynchronously this creates a probelm.
        - The CPU is very out numbered by hardware devices.
        - One solution to this is called **polling** this is where the CPU essentially goes around checking for any changes after every execution of instruction. This takes a lot of time amd is not effienct. Can also allows for dropped messages when the buffer gets filled and can't be addressed.
    - **The solution**
        - Device controllers can send **interrupts** to the CPU telling it something has happened within itself.
        - Every device has a local buffer that can hold a limited number of bytes until the CPU can respond.
    - **Interrupt Service Routine** (ISR):
        - The implementation of an interrupt or the actuall code that is executed when it jumps to it.
    - **Interrupt Vector**:
        - An array of addresses which point to code (ISR) the CPU runs to hanlde hardware requests
    - **Interrupt Request Line** (IRL)
        - A physical wire in the computer that devices can send **Interrupt signals** ac 
        - The Interrupt Request Line gets checked after **EVERY** instruction.
        - When the IRL gets signaled, it reads which interrupt is being signaled, transfers control to the intterupt service routine (ISR) **then** the ISR uses a 
        indexed vector to execute the code for the specific ISR being singaled.
    - **Terms**
        - A device **raises an intterupt**
        - The CPU **catches and dispatches** to interrrupt handlers
        - The handler clears the intterrupt when finished and the CPU goes back to executing instructions.
    - OS's are genereally considered **interrupt driven** any interaction with the system creates an interrupt that the OS must respond.
    - The interrupt architcture must save the addres of the instruction being executed when the interrupt occurs so it can return to that execution point.
    - Interrupts are typically disabled when handling another interrupt or some other critcal process.

- Videos
    - [Program, Interrupted](https://www.youtube.com/watch?v=54BrU82ANww)

### Computer System Architecture (1.3)
- Central Processing Unit (CPU)
    - ThE brAiNS oF thE COmPUtER.
    - Executes all instructions and does all computation.
    - Almost nothing happens that the CPU doesn't have some influence on.
- **Single Processor Systems**: 
    - Has one general purpose processing unit usually has multiple special purpose processors i.e. **video card**, **sound card**, **direct memory access (DMA)**, etc.
- **Multiple Process systems**: 
    - Most computers fall into this category
    - Can have parallel computation
    - A tightly coupled system?? (I don't know what he meant be this.)
    - Advantages
        - **Increased throughput**: how much information can be pushed through the system
            - Two CPUs does not create 100% more throughput. (Amdahl's law)
            - Able to share resources like memory and storage.
            - Increased reliability and fault tolerance.
            - Graceful degradation: slows down but doesn't die.
    - **Types of Multi-Processing**
        - **Asymmetric Multiprocessing**: Boss worker relationship
            - One (boss) CPU runs the OS kernel and delegates tasks to the other
            - The (worker) CPU is assigned user process'
        - **Symmetric** (SMP)
            - Each processor can run any task, both OS and user process'.
            - Shares memory between the CPUs.
    - **Multi-chip and multicore systems**
        - Multiple CPU cores on one chip
        - This allows for less power usage, faster communication between cores, and a shared cache between cores.
    - **Clustered Systems**: 
        - Multiple CPUs working together with non-shared memory.
        - Usually a shared storage system (Storage Area Network (SAN)).
        - Usually separate computing systems.
        - Can be in the same building or over a Network
        - Used in high performance computing, high availability, or when needing high fault tolerance.

### Operating System Operations (1.4)
- **Bootstrapping**
    - When a computer gets turned on this is the starting point, it has to know what to run first.
    - Stored in EPROM or Erasable Programmable Read Only Memory
    - Initializes hardware devices Loads the OS into memory and starts it up.
    - A very small program that allows for much bigger tasks to be completed.
- **Multiprogramming**
    - CPU is much faster than any single device or user so it can't be kept busy at all times without some sort of multiprogramming.
    - The jobs get organized into Proccess so the CPU can jump between jobs to keep busy
    - Process
        - A program in execution
        - Not necessarily on the CPU, but can be put on
    - Job scheduling determines what process when
- **Multitasking (Time Sharing)**
    - Extension of multiprogramming
    - Quickly switch between jobs so user can interact with each jobs
    - Keyboard input is slow in computer speed, so CPU can jump between jobs but give the user an appearance of instantaneous reaction.
- **Dual-mode and multi-mode Operations**
    - Allows an OS to protect itself and other system components
    - **Modes:**
        - **User Modes**
            - Some instructions cannot be executed
            - Used when user processes are run
        - **Kernel Modes**
            - All instructions can be executed
            - Used when OS processes are run
    - **Overhead**:
        - When an OS system call is made, and interrupt is generated that casuses the CPU to switch to kernel mode, which allows the OS to execute the system call. Once complete the CPU returns to user mode.
        - Switching modes costs thousands (1000's) of clock cycles

    - **CPU Timer:**
        - Countdown timer that generates an interrupt when expired
        - Allows OS to schedule another processes
        - A single process cannot dominate the system
        - Called time quantum when talking about scheduling
        - Makes for preemptive scheduling


### Resource Management (1.5)
    - OS manages all computing Resources
    - Process Management
        - Process Needs: CPU, Memory, I/O, Files, etc.
    - In a multi-CPU system process can be run at the same time (parrallelism)
    - OS responsibilities
        - Create and delete processes
        - Scheduling and resuming processes
        - Suspend and resuming processes
        - Provide process synchronization mechanisms
        - Provide process communication mechanisms
    - File System Management
        - Provide a logical view of data storage
        - Responsibilities
            - Mounting and unmounting
            - Free space Management
            - storage allocation
            - Disk scheduling
            - Partitioning
            - Protection
    - Cache Management
        - Cached data is data that is temporarily stored in a type of memeory for faster access
        - The OS must dtermine what data to cache and track what is cached
        - Magnetic disk -> Main Memory -> Cache -> Hardware Register
    - I/O Subsystem Management
        - Hides peculiarities of specific hardware devices from the user
        - Memory Management
            - Buffering
            - Caching
            - Spooling (I/O buffering mixed with other job execution)
        - Drivers for specific hardware devices

### Protection and Security (1.6)
    - Protection
        - Any mechanism for controlling access to processes or user to OS defined resources (memory, storage, etc)
    - Security
        - Defense of the system against internal and external attacks
    - Security  Triad
        - Confidentiality
            - Keeping data private only available to those that are to access it.
        - Integrity
            - Ensuring data is correct, authentic, and reliable
            - Data is trustworty
        - Availability
            - Data is accessible to authorized users at all authorized times.

### Computing Environments (1.10)
- **Types of Computing Environments**
    - **Traditional Computing**
        - Previously thought of as a workstation for each user with thin clients (terminals)
        - Computing is now thought of as accessing to a computing environments anywhere and everywhere.
    - **Mobile Computing**
        - Smartphones, primary method of computing for most
        - Texting, email, web browsing, social media, etc.
    - **Distributed Systems**
        - These systems are seperated physically, they are heterogenous, and all networked together
        - They can share file systems and other resources when necessary
        **Connections**
            - LAN (Local area network)
            - WAN (Wide area network)
            - MAN (Metropolitan area network)
    - **Client-Server Computing**
        - Large component of computing, the model responsible for many websites and online tasks.
        - Client generates request that are handled by servers
        - Computational intensive operations are handled elsewhere and sent back through a network.
    - **Peer-to-Peer Computing**
        - Nodes within a distrubted system are not distinguished from each other
        - Every node is a performing bot client and server activites. Think COD lobbies.
        - New nodes become part of the whole adding resources to everyone.
    - **Cloud Computing**
        - Software as a service (SaaS) i.e. Google Office
        - Platform as a service (PaaS) i.e. AWS
        - Infastructure as a service (PaaS) i.e. Storage and Backup
    - **Real-Time Embedded Systems**
        - These are systems with embedded chips that run some type of software be it a toaster, fridge, or something more complex like an arduino.
        - They run a type of OS and usually have critical time constraints on operations. They must be very efficient becuase some embedded systems are required to keep people alive.
