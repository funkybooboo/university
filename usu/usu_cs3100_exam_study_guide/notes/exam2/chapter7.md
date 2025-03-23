# Chapter 7

## Introduction to `ReentrantLock`
`ReentrantLock` is part of the `java.util.concurrent.locks` package and offers similar functionality to `synchronized` blocks/methods, but with more advanced capabilities and greater flexibility.

### Key Features of `ReentrantLock`
- **More control over lock acquisition**: Unlike `synchronized`, `ReentrantLock` allows for more explicit control of lock acquisition and release.
- **Condition support**: With `ReentrantLock`, we can create `Condition` objects that replace the monitor methods (`wait`, `notify`, `notifyAll`) used with `synchronized` blocks.

## Methods of `ReentrantLock`
### 1. `lock()`
- **Purpose**: Acquires the lock. If the lock is already held, the current thread will wait until it is released.
- **Behavior**: The thread calling `lock()` is guaranteed to get the lock unless it is interrupted or the lock is released by another thread.

### 2. `unlock()`
- **Purpose**: Releases the lock. This allows other threads to acquire the lock.
- **Behavior**: A thread can only call `unlock()` if it holds the lock.

### 3. `newCondition()`
- **Purpose**: Returns a new `Condition` object, which can be used to manage thread synchronization more flexibly.
- **Behavior**: A `Condition` object allows a thread to wait until a condition is met or another thread signals it.

## The `Condition` Object
A `Condition` object provides methods to control thread synchronization. It replaces the `Object` monitor methods (`wait()`, `notify()`, and `notifyAll()`), making them more explicit.

### Key Methods of `Condition`
1. **`await()`**
    - **Purpose**: Causes the current thread to wait until it is signaled or interrupted.
    - **Behavior**: This method atomically releases the lock and puts the thread into a waiting state.

2. **`signal()`**
    - **Purpose**: Wakes up one thread that is waiting on the `Condition`.
    - **Behavior**: After waking up, the thread must reacquire the lock before continuing execution.

3. **`signalAll()`**
    - **Purpose**: Wakes up all threads that are waiting on the `Condition`.
    - **Behavior**: Similar to `signal()`, but wakes up all waiting threads. Each thread must still reacquire the lock before proceeding.

## Common Synchronization Problems and Solutions

### 1. Bounded Buffer (Producer-Consumer Problem)
- **Problem**: Multiple processes need to access a shared pool of buffers, where each buffer holds one item. We need to synchronize access to these buffers.

#### Solution:
- **Mutex Lock**: Protects access to the shared resource.
- **Semaphores**:
    - `full` semaphore indicates how many buffers are full.
    - `empty` semaphore indicates how many buffers are empty.

#### Code:
- **Producer**:
    - Wait on `empty` to ensure there is space in the buffer.
    - Wait on `lock` before adding an item to the buffer.
    - Signal `lock` after adding and signal `full` to indicate the buffer has been filled.
- **Consumer**:
    - Wait on `full` to ensure there is something to consume.
    - Wait on `lock` before removing an item.
    - Signal `lock` after consuming and signal `empty` to indicate a buffer has been emptied.

#### Key Idea:
- Avoid unnecessary waiting by using semaphores to track the number of empty and full buffers.

### 2. Dining Philosophers Problem
- **Problem**: Philosophers alternate between thinking and eating, requiring two chopsticks to eat. They share chopsticks with neighbors, leading to synchronization issues such as deadlock.

#### Solution:
- **Semaphore-based Solution**:
    - Put a mutex on each chopstick.
    - Each philosopher waits for both chopsticks to be available before eating and releases them afterward.

#### Issues:
- **Deadlock**: If all philosophers grab one chopstick simultaneously, no philosopher can pick up the second chopstick, leading to deadlock.

#### Fixes:
- **Asymmetric Solution**:
    - Odd-numbered philosophers pick up the right chopstick first, and even-numbered philosophers pick up the left.
- **Allow n-1 Philosophers to Eat**: Only allow a maximum of four philosophers to attempt to eat at the same time, reducing the chance of deadlock.

#### Monitor-based Solution:
- **Pickup Method**:
    - A philosopher only picks up the chopsticks if both are available.
    - They change their state to "eating" and wait for their neighbors if necessary.

### 3. Readers-Writers Problem
- **Problem**: Several processes need to read from and write to shared data. A reader can safely access data concurrently, but a writer requires exclusive access.

#### Basic Solution:
- **Write Mutex**: Protects exclusive access for writers.
- **Read Mutex**: Controls access for readers.

#### Code:
- **Writer**:
    - Wait on `writeMutex`, perform writing, and then signal `writeMutex`.
- **Reader**:
    - Wait on `readMutex`, increment a `readCount`.
    - The first reader waits for the writer to finish, while other readers can proceed.
    - When the last reader finishes, it signals the writer to allow exclusive access.

#### Issue: Starvation of Writers (First Readers-Writers Problem)
- **Problem**: Readers can keep entering while writers wait indefinitely, leading to writer starvation.

#### Solution to Second Problem:
- **New Approach**:
    - Use `readTryMutex` and `writeSetupMutex` to control when writers can access the shared data and when readers can enter.
    - Writers are given priority to prevent indefinite waiting.

### 4. Third Readers-Writers Problem: Eliminating Starvation
- **Problem**: Ensuring no starvation for either readers or writers.

#### Solution:
- **Priority Queue**: Implement a priority queue (e.g., FIFO) to ensure fair access to readers and writers.
- **Fairness**: When a writer is active, no new readers are allowed to enter. Once the writer finishes, the next waiting reader or writer is serviced in the order they arrived.

---

## Summary
- **`ReentrantLock` and `Condition`**: These tools offer finer control over thread synchronization compared to the `synchronized` keyword.
- **Common Synchronization Problems**: Problems like the bounded buffer, dining philosophers, and readers-writers highlight the complexity of managing shared resources in concurrent systems.
- **Solutions**: Using `lock`, `unlock`, `wait`, `signal`, and semaphores, along with advanced techniques like priority queues, allows for effective management of concurrency and elimination of common issues like deadlock and starvation.
