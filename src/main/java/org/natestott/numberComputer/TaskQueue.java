package org.natestott.numberComputer;

import java.util.LinkedList;
import java.util.concurrent.locks.ReentrantLock;

class TaskQueue {
    private final LinkedList<Long> queue = new LinkedList<>();
    private final ReentrantLock lock = new ReentrantLock();

    public void addTask(Long task) {
        lock.lock();
        try {
            queue.add(task);
        } finally {
            lock.unlock();
        }
    }

    public Long getTask() {
        lock.lock();
        try {
            return queue.isEmpty() ? null : queue.removeFirst();
        } finally {
            lock.unlock();
        }
    }
}
