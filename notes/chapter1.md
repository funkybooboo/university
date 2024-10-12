## Chapter 1

### What are Operating Systems (1.1)

- An operating system is a **program** that sits between the user and the computer hardware.
- It manages computer hardware to run user programs.
- It allows for the use of computer hardware in an easy and convenient way.
- An OS's main jobs
    - Allocate Resources
        - Manages all resources.
        - Manage conflicting requests for efficient and fair use.
        - Schedules tasks to run on the CPU and manages hardware interrupts
        - One of the goals of an OS is to use hardware **efficiently**. 
    - Control Program
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
- Main Memory: The only large storage the CPU can access directly, typically volatile or will lose its data when power is lost.
    - Random Access: typically Dynamic Random Access Memory (DRAM)
        - Can access any location at any time. Doesn't need to read linearly to get to an address.
- Secondary Storage: Much larger chunk of memory, non-volatile, much slower.
    - Mechanical: Hard disk drives (HDD) with platters.
        - The disk controller manages the interface between the device and the OS.
    - Electrical: Solid state drives (SSD). Much faster than HDD.
    - Both examples of (NVM) non-volatile memory.
- Storage Hierarchy: Organized by speed, cost, and volatility (Fastest on top)
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
    - The problem
        - The CPU can do only one thing at a time and all the hardware it is managing is operating asynchronously this creates a probelm.
        - The CPU is very out numbered by hardware devices.
    - The solution
        - Device controllers can send **interrupts** to the CPU telling it something has happened within itself.
        - Every device has a local buffer that can hold a limited number of bytes until the CPU can respond.
    - Interrupt Service Routine (ISR): is the implementation of an interrupt
    - Interrupt Vector: An array of addresses which point to code (ISR) the CPU runs to hanlde hardware requests
    - The CPU has a physical wire called the Interrupt Request Line (IRL)
        - The Interrupt Request Line gets checked after **EVERY** instruction.
        - When the IRL gets signaled, it reads which interrupt is being signaled, transfers control to the intterupt service routine (ISR) **then** the ISR uses a 
        indexed vector to execute the code for the specific ISR being singaled.
    - Terms
        - A device **raises an intterupt**
        - The CPU **catches and dispatches** to interrrupt handlers
        - The handler clears the intterrupt when finished and the CPU goes back to executing instructions.

    - OS's are genereally considered **interrupt driven** any interaction with the system creates an interrupt that the OS must respond.

    - The interrupt architcture must save the addres of the instruction being executed when the interrupt occurs so it can return to that execution point.

    - Interrupts are typically disabled when handling another interrupt or some other critcal process.


### Computer System Architecture (1.3)
- Central Processing Unit (CPU)
    - ThE brAiNS oF thE COmPUtER.
    - Executes all instructions and does all computation.
    - Almost nothing happens that the CPU doesn't have some influence on.
- Single Processor Systems: Has one general purpose processing unit usually has multiple special purpose processors i.e. **video card**, **sound card**, **direct memory access (DMA)**, etc.
- Multiple Process systems: Most computers fall into this category
    - Can have parallel computation
    - A tightly coupled system?? (I don't know what he meant be this.)
    - Advantages
        - Increased throughput: how much information can be pushed through the system
            - Two CPUs does not create 100% more throughput. (Amdahl's law)
            - Able to share resources like memory and storage.
            - Increased reliability and fault tolerance.
            - Graceful degradation: slows down but doesn't die.
    - Types of Multi-Processing
        - Asymmetric Multiprocessing: Boss worker relationship
            - One (boss) CPU runs the OS kernel and delegates tasks to the other
            - The (worker) CPU is assigned user process'
        - Symmetric (SMP)
            - Each processor can run any task, both OS and user process'.
            - Shares memory between the CPUs.
    - Multi-chip and multicore systems
        - Multiple CPU cores on one chip: this allows for less power usage, faster communication between cores, and a shared cache between cores.
    - Clustered Systems: Multiple CPUs working together with non-shared memory.
        - Usually a shared storage system (Storage Area Network (SAN)).
        - Usually separate computing systems.
        - Can be in the same building or over a Network
        - Used in high performance computing, high availability, or when needing high fault tolerance.

### Operating System Operations (1.4)

### Protection and Security (1.6)

### Computing Environments (1.10)

