package org.natestott.numberComputer;

import java.util.HashMap;
import java.util.concurrent.locks.ReentrantLock;

class ResultTable {
    private final HashMap<Long, Integer> results = new HashMap<>();
    private final ReentrantLock lock = new ReentrantLock();

    public void putResult(Long position, Integer digit) {
        lock.lock();
        try {
            results.put(position, digit);
        } finally {
            lock.unlock();
        }
    }

    public HashMap<Long, Integer> getResults() {
        return new HashMap<>(results);
    }
}
