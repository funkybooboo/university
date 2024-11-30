# Chapter 10: Virtual Memory

### Purpose
- Describe the benefits of a **virtual memory system**.
- Explain the concepts of **demand paging**, **page-replacement algorithms**, and **allocation of page frames**.
- Examine the relationship between **shared memory** and **memory-mapped files**.
- Explore how **kernel memory** is managed.

## Background (10.1)

### The Need for Virtual Memory

- **Code execution**: For a program to execute, it must be loaded into memory, but **not all parts of the program** are needed at once.
    - Certain parts of the code handle **error conditions** or other exceptional cases.
    - Arrays or lists may not use all the memory allocated to them.
    - Features or options within a program may only be used **rarely**, and don't need to be in memory all the time.

- **Partial loading of programs**: Often, the entire program is not needed in memory at once.
    - This **allows for more efficient memory usage**.
    - Programs don't need to fit entirely into physical memory at once, enabling the system to run more programs concurrently.

- **Benefits of Partial Loading**:
    - **Less I/O swapping**: Instead of swapping entire processes in and out of memory, only the needed parts of programs (like individual pages) are swapped.
    - **Faster startup**: Since only parts of the program are loaded initially, the program starts faster.

### Virtual Memory

- Virtual memory is a **separation of logical memory** (what a program sees and works with) from physical memory (actual RAM).
- This concept is an extension of the ideas covered in the previous chapter, and it is implemented through **demand paging**.
    - **Logical memory**: This is the address space that the process believes it has available.
    - **Physical memory**: This is the actual physical RAM, which may not be sufficient to hold the entire logical memory.

#### Memory Layout of a Process

- A typical process consists of:
    - **Max**: Maximum address space.
    - **Stack**: Used for function calls and local variables.
    - **Heap**: Dynamically allocated memory.
    - **Data**: Global variables.
    - **Code**: The program code itself.

- Often, there is a **big hole** between the **stack** and **heap** to allow for dynamic size changes.
    - This part of memory may not need to be loaded into physical memory at all if there is no data there, leading to **better memory utilization**.

### Key Concept: **Demand Paging**
- With **demand paging**, only the required pages of a program are loaded into physical memory when needed. This concept underlies virtual memory systems.
- This approach helps manage memory more efficiently, as parts of the program can be swapped in and out of memory based on need, rather than loading the entire program upfront.


## Demand Paging (10.2)

### Key Concepts of Demand Paging

- **Demand paging**: Pages are brought into memory only when they are needed.
    - If a page is never accessed, it is never loaded into memory.

### Hardware Support for Demand Paging

- The **Page Table** tracks which pages are in memory and which are not.
    - **Valid/Invalid bit**:
        - This bit is used to indicate whether a page is currently in memory (valid) or not (invalid).
        - Previously, the valid/invalid bit only tracked unused entries, but now it serves to track the status of pages in memory.
        - The **Process Control Block (PCB)** contains a table that helps track if a page is associated with data.

#### Accessing Memory

1. **If the page is in memory**, the memory is accessed as normal.
2. **If the page is not in memory**:
    - The page entry is marked as **invalid**.
    - A **page fault** occurs, and the OS handles the page fault to bring the page into memory.

### Page Fault Handling

1. **Page fault detection**: The OS checks if the page is genuinely invalid or if it simply hasn't been loaded into memory yet.
    - If it's invalid, the process is stopped.
    - If the page is just not in memory, the OS steps in to handle it.
2. **OS actions**:
    - Find a **free frame** in memory.
    - **Read the page** from storage (such as a backing store or disk) into the physical memory.
    - Update the **Page Table** and **PCB** to reflect the new memory status.
    - **Restart the interrupted instruction** that triggered the page fault.

### Pure Demand Paging

- **Pure demand paging**: A page is brought into memory **only when needed**.
    - It is possible to start a program with a **page fault** to force the loading of the first required page.
    - **Issue**: A single instruction could potentially trigger multiple page faults (e.g., one for the instruction and others for data).
    - However, this is rare due to **locality of reference**: memory access tends to be concentrated in a small area of the logical address space (same page).

### Managing Free Frames

- The OS needs to manage the **list of available frames**:
    - This list can be implemented as a **linked list**.
    - Since it doesn’t matter which frame a page is assigned to, there’s no complex strategy for selecting a frame.
    - **Zero-fill-on-demand**: The content of a frame is erased (filled with zeros) before it is reassigned to a new page, ensuring no security risks from leftover data.

### Performance of Demand Paging

The **Effective Access Time (EAT)** measures the average time it takes to access memory, factoring in the occurrence of page faults.

- **Memory Access Time (MA)**: The time to access memory when there is **no page fault**.
    - Assumed to be **200 ns**.

- **Page Fault Probability (P)**: The likelihood of encountering a page fault.
    - Typically a small value (between 0 and 1).

- **Page Fault Time (PFT)**: The time taken to handle a page fault (i.e., read the page from disk and load it into memory).
    - Assumed to be **8 ms** (8,000,000 ns).

The formula for **Effective Access Time (EAT)** is:

\[
EAT = (1 - P) \times MA + P \times PFT
\]

#### Example Calculation

If the probability of a page fault is \(P = 0.001\) (1 in 1000 accesses), we can calculate the EAT:

\[
EAT = (1 - 0.001) \times 200 + 0.001 \times 8000000
\]
\[
EAT = 200 - 0.2 + 8000 = 8200 \, \text{ns}
\]

This results in an **EAT of 8.2 microseconds** (8200 ns), which is a significant slowdown compared to the normal 200 ns access time.

To keep the degradation under 10%, the page fault rate must be **less than 0.000025** (1 page fault every 400,000 memory accesses), ensuring the EAT remains below 220 ns.

### Optimizing Demand Paging

To optimize performance, it's essential to **minimize page fault rates**, as frequent page faults significantly slow down memory access.

- **Backing store**: It is faster to read from the backing store than from the filesystem, so it is beneficial to maximize backing store usage.

- **Old Approach**: Copying the entire process into the backing store at load time. While this simplifies management, it adds the overhead of copying the entire process.



## Copy On Write (COW) (10.3)

### Overview of **Copy On Write (COW)**

- **fork() system call**: When a process calls **fork()**, a new process is created. Initially, this process is a duplicate of the parent process, including its memory space.
- The **first impression** of forking might suggest that memory is duplicated for the child process, but that's not entirely accurate.
- **COW** is an optimization technique used to handle this duplication efficiently.

### How Copy On Write Works

1. **Shared Pages**:
    - After **fork()** is called, the child process shares the same memory pages as the parent.
    - Instead of creating a completely new memory space for the child, **both processes** (parent and child) use the **same pages**. This means memory is not duplicated, making the process creation **very fast** and **memory efficient**.

2. **Memory Efficiency**:
    - Since no actual memory copying happens initially, the **memory usage is efficient**—no physical memory is wasted.
    - If the child process makes an **exec()** call shortly after the fork, there is no additional cost from the unnecessary duplication of memory.

3. **Shared Memory Until Write**:
    - The shared memory between the parent and child is marked as **COW**.
    - **COW behavior**: The page is marked as **copy-on-write**, which means that the actual duplication of memory occurs only when one of the processes (either parent or child) attempts to **write** to the page.
    - If a process writes to the page:
        - The page is **copied** (the write only happens on the page for the process that made the write).
        - The other process (which hasn't written) continues to share the original page.

4. **Write Happens on Process’s Own Copy**:
    - The write occurs only on the page that is **associated with the writing process**. This prevents both processes from writing to the same page at once, ensuring consistency and independence of changes.

### Example

- **Process A** (Parent) calls **fork()**, creating **Process B** (Child).
- Initially, **Process A** and **Process B** share the same memory pages.
- If **Process A** writes to one of the pages, that page is copied, and **Process A** will now have its own private copy of the page.
- Similarly, if **Process B** writes to the same page, a new copy of the page will be created for **Process B**.

### Benefits of COW

1. **Efficient Memory Use**: Memory is not duplicated unnecessarily. Both processes share the same page until a modification is made.
2. **Performance**: The system avoids the overhead of copying large amounts of memory during the `fork()` process, improving the performance and speed of process creation.
3. **Reduced Overhead**: Only when a page is modified does the system incur the cost of copying it, making it more efficient than simply duplicating memory right away.

COW is a key optimization used by many operating systems to efficiently handle process creation and memory management.

## Page Replacement (10.4)

### Over-Allocation and the Need for Page Replacement

- **Virtual Memory**: Virtual memory allows a system to over-allocate memory, meaning a system can have more logical pages than physical memory frames available.

  **Example**:
    - A system has 40 frames of physical memory.
    - There are 10 processes, each requiring 6 pages (60 pages in total).
    - Initially, each process uses 3 pages (30 pages used).
    - Each process demands 2 more pages, resulting in a total of 50 pages needed.
    - The system runs out of space because there are more logical pages in use than the physical memory can handle.

- Since there is **more logical memory** (pages) required than physical memory frames, **page replacement** is necessary to manage this discrepancy.

- **OS and I/O buffers** also consume memory, so the total memory available to user processes is even smaller than the total physical memory.

### Basics of Page Replacement

- **When page replacement is needed**:
    - When a process tries to load a page but there is no free frame available.
    - A **victim page** is selected to be removed, and the page is swapped out to the backing store (e.g., hard drive).
    - The page table, Process Control Block (PCB), and Frame table are updated to reflect that the page is no longer in memory.
    - The required page is then loaded from disk into a free frame, and the page tables are updated again.

- **Performance Issues**:
    - **Page replacement is expensive** because it involves two page writes: one to swap out the victim page and another to swap in the new page.

- **Optimization Strategies**:
    - **Modify bit (Dirty bit)**:
        - Marks if a page has been modified in memory.
        - If the page is dirty, it must be written to the backing store when it is swapped out.
        - If the page is clean (not modified), there is no need to write it back to the backing store, saving time.

    - **Read-Only Memory**:
        - Pages that are read-only do not need to be swapped.
        - If a process writes to a read-only page, the system simply overwrites the page and re-reads it from the filesystem the next time it is used.
        - This saves time by avoiding unnecessary writes to the swap space.

### Page Replacement Algorithm Issues

- **Key Problems**:
    - **Which page to replace?** This is the core of the page replacement problem.
    - **How many frames should each process be allocated?** This impacts the overall efficiency of memory usage.

- **Goal**: The goal of page replacement is to minimize **page faults**, especially **first-time accesses** (cold-start page faults) and subsequent accesses of a page.

- **Evaluation**: Algorithms are often evaluated based on a **reference string**, which represents the order in which pages are accessed by the process.

### Page Replacement Algorithms

- **Reference String**: For example, consider a reference string:
  ```
  7, 0, 1, 2, 0, 3, 0, 4, 2, 3, 0, 3, 2, 1, 2, 0, 1, 7, 0, 1
  ```
  This represents the order in which pages are accessed.

#### 1. **First-In-First-Out (FIFO)**

- **How it works**: The first page to be loaded into memory is the first to be replaced when a page fault occurs. This simple algorithm assumes that older pages are less likely to be used.

- **Example with 3 frames**:
    - After processing the reference string, FIFO results in **15 page faults**.

- **Behavior**:
    - FIFO can sometimes lead to poor performance if a page is used repeatedly but is swapped out early because it was the first to be loaded into memory.

#### 2. **Optimal Page Replacement (OPT)**

- **How it works**: This algorithm replaces the page that will not be used for the longest time in the future.

- **Ideal but impractical**: OPT requires knowledge of future page accesses, which is not possible in real systems.

- **Example**:
    - Using the same reference string, OPT results in **9 page faults**.

- **Purpose**: The optimal algorithm serves as a baseline to evaluate other, more practical algorithms.

#### 3. **Least Recently Used (LRU)**

- **How it works**: The page that hasn't been used for the longest time is replaced. Each time a page is accessed, it is moved to the end of the list of pages.

- **Example with 3 frames**:
    - The reference string results in **12 page faults**.

- **Behavior**:
    - LRU is considered one of the most efficient and widely used algorithms because it assumes that pages recently used are likely to be needed again soon.

### Revisit FIFO: **Belady's Anomaly**

- **FIFO with More Frames**: Interestingly, increasing the number of frames does not always reduce the number of page faults.
    - **Example**: For the reference string `1, 2, 3, 4, 1, 2, 5, 1, 2, 3, 4, 5`:
        - **3 frames** result in 9 page faults.
        - **4 frames** result in 10 page faults.

- **Belady's Anomaly**: This counterintuitive behavior, where increasing the number of frames increases the page fault rate, is known as **Belady's Anomaly**.

- **Conclusion**: While it may seem that providing more memory should reduce page faults, certain algorithms (like FIFO) can behave unpredictably, leading to more faults with additional memory.

### Conclusion

Page replacement is a critical part of managing virtual memory, especially when a system is over-allocated. The choice of algorithm can significantly affect system performance. While algorithms like **LRU** and **OPT** generally perform well, simpler ones like **FIFO** can show unexpected behaviors, such as **Belady's Anomaly**, where adding more frames results in more page faults.


## Allocation of Frames (10.5)

### How Do We Allocate a Fixed Amount of Memory Among Processes?

When managing virtual memory, the system must decide how to allocate a limited amount of physical memory (frames) among all the running processes. There are several strategies for doing this, and each has its trade-offs in terms of performance, system throughput, and fairness.

### Allocation Algorithms

1. **Equal Allocation**
    - **Description**: Each process is allocated the same number of frames.
    - **Advantages**: Simple and easy to implement.
    - **Disadvantages**:
        - Small processes that don't need all the frames may end up with excess unused memory, leading to wasted space.
        - Large processes may end up with a relatively small number of frames, causing them to swap more frequently, potentially degrading performance significantly.

2. **Proportional Allocation**
    - **Description**: Frames are allocated based on the size of the virtual address space of each process (i.e., the amount of virtual memory that the process can address).
    - **Advantages**: This method ensures that larger processes, which need more memory, are given more frames.
    - **Disadvantages**: Small processes may still end up swapping a lot because they receive a proportionally small number of frames, even though they may not require much memory.

3. **Priority Allocation**
    - **Description**: Frames are allocated based on the priority of the process. High-priority processes are given more frames, while lower-priority processes are given fewer.
    - **Advantages**: This allows the system to give more resources to more critical processes.
    - **Disadvantages**:
        - Low-priority processes may end up with fewer frames, even if they are large, and this can increase their swapping frequency.
        - Can be combined with the proportional allocation, where the priority influences how frames are distributed relative to process size.

### Global vs. Local Allocation

- **Local Allocation**:
    - **Description**: Page replacement is limited to only the frames allocated to a specific process.
    - **Advantages**:
        - Provides **consistent per-process performance** because each process can only use its allocated frames.
        - Helps prevent a process from being unfairly starved of memory by other processes.
    - **Disadvantages**:
        - Can lead to **underutilized memory** if a process doesn’t need all the frames allocated to it. This may increase swapping, as frames are available only to the specific process.

- **Global Allocation**:
    - **Description**: Page replacement can select frames from the entire pool of physical memory (i.e., it is not restricted to the frames allocated to a specific process).
    - **Advantages**:
        - Results in **higher system throughput** because frames can be dynamically reallocated among processes.
        - Allows a process to "steal" frames from other processes if needed, potentially reducing overall system swapping.
    - **Disadvantages**:
        - The performance of a process can **vary** from one execution to another, as other processes may take up more memory, leaving less for the current process. This introduces some variability in execution time.
        - This method can lead to **unfairness**, as lower-priority processes can be starved of memory if higher-priority processes require more.

### Summary

- **Equal Allocation** is simple but inefficient for processes of different sizes.
- **Proportional Allocation** gives processes frames based on their size but can still cause swapping for smaller processes.
- **Priority Allocation** is useful for giving more memory to important processes but may degrade the performance of low-priority processes.
- **Local Allocation** ensures that each process gets a consistent amount of memory but can lead to wasted space and more swapping.
- **Global Allocation** increases system throughput by allowing frames to be shared across processes, but it can lead to performance variability and unfairness between processes. It is generally more common in systems to optimize overall system performance.

Each method has its strengths and weaknesses, and the choice between global and local allocation depends on the desired balance between fairness and overall system throughput.



## Thrashing (10.6)

### What is Thrashing?

Thrashing occurs when a system spends more time paging (i.e., swapping data between physical memory and disk) than executing actual processes. This happens when processes do not have enough memory allocated to hold their "active" pages, which are frequently accessed by the CPU. As a result, pages are swapped in and out of memory rapidly, causing a significant slowdown in system performance.

### How Thrashing Happens:

1. **Inadequate Number of Frames**:
    - When a process doesn't have enough memory (frames) for its active pages, it can’t keep all the necessary data in memory.

2. **Page Faults**:
    - As the process tries to access pages that are not in memory, page faults occur.
    - Each page fault triggers an I/O operation to fetch the page from the backing store (disk).

3. **The Cycle of Thrashing**:
    - **High Paging**: If multiple processes experience page faults, the system spends a lot of time swapping pages in and out of memory.
    - **High I/O**: As a result of frequent page faults, the system experiences significant I/O activity (reading and writing pages from/to the disk).
    - **Low CPU Utilization**: During this I/O-heavy phase, the CPU is often idle, waiting for data to be swapped in or out, leading to decreased CPU utilization.
    - **Increased Multiprogramming**: To compensate for the decreased CPU utilization, the operating system might try to add more processes to the system (increase multiprogramming) in an attempt to keep the CPU busy. This only exacerbates the issue because more processes mean more competition for the limited available memory.

4. **The Result**:
    - The system spends more time paging (i.e., performing disk I/O) than actually executing the processes, leading to **severe performance degradation**.

### Solutions to Thrashing

1. **Establish an Acceptable Page Fault Rate**:
    - The system should monitor the page fault rate and ensure it stays within acceptable limits.
    - If the page fault rate is too **low**, it indicates that a process is not using enough of its allocated memory, and the OS may consider **removing a frame** from the process.
    - If the page fault rate is too **high**, it means the process doesn't have enough memory to operate efficiently, and the OS may **add a frame** to the process to reduce page faults.

2. **Use Local Page Replacement**:
    - Local page replacement policies (such as keeping each process's page replacement contained within the frames allocated to that process) may help limit the scope of thrashing. However, this approach can have its own drawbacks, such as inefficient use of memory for small processes or excessive swapping if a process is allocated too few frames.

3. **Add More Memory**:
    - If possible, adding more physical memory to the system can reduce thrashing by providing more space for processes to keep their active pages in memory, reducing the frequency of page faults.

4. **Terminate Processes**:
    - In extreme cases, the operating system may terminate processes to free up memory. Terminating the least critical or lower-priority processes can help allocate more frames to processes that need them and reduce overall system paging.

### Summary

Thrashing is a state where the system becomes overloaded with paging activity, leading to inefficient CPU usage and poor overall performance. It typically occurs when processes don't have enough memory to hold their active pages. Solutions to thrashing involve managing page fault rates, optimizing memory allocation strategies, adding memory, or terminating processes. Preventing thrashing requires careful monitoring and adjustment of memory usage and system processes to balance CPU utilization and memory demands.


## Other Considerations (10.9)

### 1. Pre-paging

**Pre-paging** is a technique used to improve the performance of a system by preloading some pages into memory when starting a process, even before they are actually requested. This contrasts with **pure demand paging**, where pages are loaded only when they are accessed for the first time.

#### Advantages of Pre-paging:
- **Fewer page faults at startup**: Pre-paging can reduce the number of page faults that occur when a process begins execution. Since some of the pages are already loaded into memory, the operating system doesn’t need to fetch them one by one as the program runs.
- **Faster startup**: Pre-paging can improve the startup time of a process since pages are loaded in bulk, reducing the number of **context switches** and overhead from loading pages one at a time.

#### Disadvantages of Pre-paging:
- **Wasted I/O and memory**: Not all pages preloaded into memory may be used by the process. This could lead to unnecessary use of memory and I/O operations for pages that never get accessed, which can waste system resources.

### 2. Page Size

The choice of **page size** significantly impacts the performance and efficiency of memory management.

- **Small pages**:
    - **Less internal fragmentation**: Smaller pages reduce memory wastage due to unused space within a page.
    - **More page faults**: Smaller pages lead to more frequent page faults because smaller amounts of data are loaded into memory at a time. This can lead to higher overhead due to the increased frequency of paging operations.

- **Large pages**:
    - **Fewer page faults**: Larger pages reduce the number of page faults since a larger chunk of memory is loaded in one operation. This can improve performance by reducing the need for paging.
    - **More internal fragmentation**: Larger pages can cause more memory wastage due to unused space within the page. This is because a larger page may contain more memory than the process actually needs.

Choosing an optimal page size is a trade-off between the frequency of page faults and the level of internal fragmentation.

### 3. TLB Reach

**TLB Reach** refers to the amount of memory that can be efficiently accessed through the **Translation Lookaside Buffer (TLB)**. The TLB is a small, fast cache that stores recently used memory page table entries. When the TLB can hold more of the memory pages that the CPU needs, memory access is faster.

- **TLB Reach** is calculated as:
  \[
  \text{TLB Reach} = \text{TLB Entries} \times \text{Page Size}
  \]

#### Implications of TLB Reach:
- **Larger page sizes** increase TLB Reach because each entry in the TLB can map to a larger portion of memory. This reduces the likelihood of TLB misses, improving memory access speed.
- **Smaller page sizes** reduce TLB Reach, potentially causing more TLB misses but using less memory and reducing the likelihood of internal fragmentation.

However, larger pages may cause **more internal fragmentation** (wasting memory) and could increase the time needed to swap pages in and out of memory.

### 4. IO Interlock

**I/O Interlock** is a technique used to ensure that certain pages are **locked into memory** while I/O operations are being performed on them. This prevents situations where a page could be swapped out to disk while an I/O operation is in progress, which could result in data corruption or inconsistencies.

- If a page is swapped out while I/O is writing to it, it could cause **overhead** and errors, as the I/O operation may be trying to write to a page that has been moved or is no longer in memory.

- To manage this, the operating system must ensure that I/O operations are carefully synchronized with memory paging to avoid such conflicts. The management of these locked pages creates **additional overhead** but is necessary for system stability and data integrity.

### Summary of Key Points:
- **Pre-paging** can reduce startup time and page faults but may waste memory and I/O resources.
- The **size of memory pages** affects internal fragmentation and page fault frequency, requiring a trade-off between the two.
- **TLB Reach** increases with larger page sizes, improving access speed but potentially wasting memory and increasing swap times.
- **I/O Interlock** ensures that pages being written to during I/O operations are not swapped out, preventing errors and avoiding unnecessary overhead in the system.
