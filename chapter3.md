## Chapter 3

### Process Concept (3.1)

#### Definition of a Process

- A **process** is an instance of a program in execution.
- It represents a program that has been loaded into RAM and is currently being executed by the CPU.
- A process serves as a unit of work within an operating system and contains:
    - **Threads**: Each process can have one or more threads. If it has no threads, the OS will terminate the process.
        - A thread feeds instructions to the CPU
    - **Security Token**: This defines the permissions of the process, specifying what resources it can access.
    - **Code (Text)**: The executable instructions of the program.
    - **Constants (Data)**: Fixed values used by the program.
    - **Private Virtual Address Space**: The memory space allocated to the process, which isolates it from other
      processes.
        - **Stack**
        - **Heap**
    - **Private Handle Table**: A structure that keeps track of system resources (like files and network connections)
      that the process can use.
- A process encapsulates all necessary information for executing the program.

#### Definition of a Process Control Block (PCB)

- The **Process Control Block (PCB)** is a data structure maintained by the operating system to manage and track a
  process's execution.
- A PCB contains critical information such as:
    - **State**: The current state of the process (e.g., running, waiting).
    - **UID**: A unique identifier for the process.
    - **Privileges**: The resources the process is authorized to access.
    - **Program Counter**: Indicates the next instruction to be executed.
    - **CPU Registers**: The values of the CPU registers when the process was last executed.
    - **CPU Scheduling Info**: Includes priority level and pointers for scheduling queues.
    - **Virtual Memory Info**: Details about the virtual memory allocated to the process.
    - **Parent Process ID**: Identifies the process that created it.
    - **I/O Info**: A list of open file descriptors.

#### Relationship between Process and Process Control Block

- **Representation**: The PCB represents the process to the operating system, encapsulating its state and attributes,
  but is not the process itself.
- **Management**: The OS uses the PCB to manage a process's lifecycle, switching between states (e.g., ready, running,
  blocked) and scheduling it for CPU time.
- In summary, while a process is the active entity performing tasks, the PCB is a data structure containing all
  necessary information to manage that process. They are interconnected but distinct entities.

#### Process States

- A process can exist in various states:
    - **New**: The process is being created.
    - **Ready**: The process is prepared to run but is waiting for CPU time.
    - **Running**: The process is currently being executed by the CPU.
    - **Terminated**: The process has finished execution.
    - **Waiting**: The process is waiting for an event (e.g., I/O operation) to complete.

- The **Process Scheduler** or **Dispatcher** in the operating system examines all processes in the ready state and
  decides which process
  will run on the CPU.
- A running process may be preempted and moved back to the ready state or may terminate upon completion.

![Process State Life Cycle](images/chapter3/process_states.jpg)

#### Additional Resources

- [What is a Process? Video Explanation](https://www.youtube.com/watch?v=vLwMl9qK4T8)
- [What is a Process? Video Explanation](https://www.youtube.com/watch?v=LAnWQFQmgvI)

### Process Scheduling (3.2)

### Operations on Processes (3.3)

### Interprocess Communication (3.4)

### Shared Memory Systems (3.5)

### Message Passing Systems (3.6)
