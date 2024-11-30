# Operating System (OS) Overview

## OS Overview

This section provides a general overview of what happens in an OS. It covers fundamental processes and steps, though variations exist in specific systems.

### Steps in the OS Process:
1. **Threads/Processes**
2. **Booting**
3. **First Process (pid=0)**
4. **Many Processes/Accessing Memory**
5. **Executing Instructions**
6. **Allocating CPU**

## Threads and Processes

- **OS Management**: The OS primarily manages threads, not processes.
- **Process Structure**: A process has a stack, heap, data, and text sections, with various registers and files associated with it.
    - **Single-threaded Process**: A process with one thread of execution.
    - **Multithreaded Process**: A process containing multiple threads, allowing concurrent execution.

- **One-to-One Model**: This model is commonly used where each user-level thread maps to one kernel-level thread.

### Process Control Block (PCB)
- **Attributes**: A PCB contains the following for each process:
    - Process state
    - Process number (PID)
    - Program counter
    - Registers
    - Memory limits
    - List of open files

### Thread Control Block (TCB)
- Each thread has a TCB with unique information, including a pointer to the PCB. It holds similar data as the PCB, relevant only to the thread's execution.

### Threads and Concurrency
- Threads within a process share memory and resources, enabling concurrency.

## Boot Process

- **UEFI** (Unified Extensible Firmware Interface) and **GPT** (GUID Partition Table) are modern standards for booting and partitioning.
- The **GUID** (Globally Unique Identifier) helps track the location of the OS on the drive.

### First Process (pid=0)
1. The OS boots and the first process (pid=1) starts.
2. **Pure Demand Paging**: The first instruction is validated, and a logical address is checked.
3. **Memory Management**:
    - The system checks the page table, and if the page is not in memory (TLB miss), it loads it from disk into memory.
    - Interrupts are generated for I/O, and once the I/O completes, the process moves to the Ready state.
4. The **process tree** is established as the first process (pid=1) is loaded.

## Many Processes/Accessing Memory

1. **Thread Scheduling**: A thread is assigned to the CPU, its register values are loaded, and the instruction pointer is validated.
2. **Page Table Management**:
    - If the page isn't in memory, the OS will check the free frame list.
    - If no frames are available, a victim frame is selected for swapping.
    - During I/O operations, the thread enters a wait state, and the OS processes the interrupt when completed, moving the thread back to the Ready state.

## Executing Instructions

1. **System Calls**: When a system call is made, parameters are passed via registers or stack.
2. **Context Switch**: A trap switches from user mode to kernel mode for system call execution.
3. **Memory Writes**: The system validates that a memory write occurs at a valid, writable location.
4. **I/O Operations**: For I/O requests, processes are moved to the wait state, and once completed, an interrupt is processed to bring them back to Ready.

### Process Creation
1. **Fork**:
    - A new PCB and TCB are created for the new process.
    - **Copy-on-write (COW)** optimizes memory usage by only copying memory when modifications occur.
    - The new TCB is placed in the Ready queue.

### Thread Creation
- Creating threads is cheaper than creating processes as they share memory. A new TCB is created, and the thread is placed in the Ready queue.

### Data Sharing
- **Processes** use interprocess communication (IPC), including shared memory and message passing.
- **Threads** within a process share memory space, and synchronization mechanisms are needed for thread safety.

## Allocating CPU

### Non-Preemptive Scheduling
- When a CPU burst completes, it signals a trap. The OS handles the context switch.
- On process completion, the OS performs maintenance tasks like releasing pages and cleaning up the PCB.

### Preemptive Scheduling
- A timer triggers a context switch, saving the current process state in its PCB, handling the interrupt in kernel mode, running the scheduler, and loading the next process onto the CPU.

### Scheduling Algorithms
- The OS picks a thread from the ready queue using various algorithms.
- **Issues**:
    - Starvation
    - Priorities
    - Performance (minimizing average wait, maximizing throughput)

## Security Overview

### The Security Problem
- Any system can be a target of an attack, with motivations ranging from personal data theft, monetary gain, to sheer thrill.

### Definitions
- **Secure**: A system is secure if all its resources are used and accessed as intended.
- **Violations**: Unauthorized usage of a system's resources, either accidental or malicious.
- **Attacker**: Any entity attempting to compromise system security (e.g., hackers).
- **Threat**: The potential for a security violation to occur.
- **Attack**: An actual attempt to breach security.
- **Vulnerability**: A weakness in the system that can be exploited.
- **Exploit**: A specific action that takes advantage of a system's vulnerability.

### Types of Attacks

#### **Breach of Confidentiality**
- Unauthorized access to or reading of sensitive data.

#### **Breach of Integrity**
- Unauthorized modification of data, questioning its correctness.

#### **Breach of Availability**
- Denial of access to authorized users at the expected times.

These three breaches form the **CIA Triad**:
- **Confidentiality**
- **Integrity**
- **Availability**

### Example Attacks

1. **Masquerading**: Impersonating an authorized user, violating authentication and affecting confidentiality and integrity.
2. **Replay Attack**: Replaying old messages, usually to alter or disrupt transactions (e.g., in banking).
3. **Man-in-the-Middle**: The attacker intercepts and possibly alters communication between two parties. It may involve:
    - Eavesdropping
    - Message modification
    - Injection or dropping of messages

### Countermeasures and Cryptography
- The fundamentals of cryptography (encryption, authentication, hashing) are key in protecting systems from the attacks mentioned above. Countermeasures include implementing strong security protocols, regular updates, and intrusion detection systems.

