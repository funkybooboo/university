package org.natestott;

import java.util.LinkedList;
import java.util.concurrent.locks.ReentrantLock;

/**
 * The TaskQueue class provides a thread-safe queue for managing tasks
 * to be processed. It allows multiple threads to add tasks to the queue
 * and retrieve them safely, ensuring that access to the queue is synchronized.
 *
 * <p>This class uses a LinkedList to store tasks and employs a
 * ReentrantLock to manage concurrent access, preventing race conditions.</p>
 *
 * @author Nate Stott
 */
public class TaskQueue {

    private final LinkedList<Long> queue = new LinkedList<>();
    private final ReentrantLock lock = new ReentrantLock();

    /**
     * Adds a new task to the end of the queue.
     *
     * @param task The task to be added to the queue.
     */
    public void addTask(Long task) {
        lock.lock();
        try {
            queue.add(task);
        } finally {
            lock.unlock();
        }
    }

    /**
     * Retrieves and removes the first task from the queue.
     *
     * @return The first task in the queue, or null if the queue is empty.
     */
    public Long getTask() {
        lock.lock();
        try {
            return queue.isEmpty() ? null : queue.removeFirst();
        } finally {
            lock.unlock();
        }
    }
}
