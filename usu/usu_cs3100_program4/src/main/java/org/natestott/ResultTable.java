package org.natestott;

import java.util.HashMap;
import java.util.concurrent.locks.ReentrantLock;

/**
 * The ResultTable class serves as a thread-safe container for storing
 * the results of computed digits associated with their respective positions.
 *
 * <p>This class uses a HashMap to maintain the results and employs
 * a ReentrantLock to ensure thread safety when multiple threads access
 * or modify the results.</p>
 *
 * @author Nate Stott
 */
public class ResultTable {

    private final HashMap<Long, Integer> results = new HashMap<>();
    private final ReentrantLock lock = new ReentrantLock();

    /**
     * Stores the computed digit at the specified position in the results table.
     *
     * @param position The position of the digit being stored.
     * @param digit The computed digit to be stored at the specified position.
     */
    public void putResult(Long position, Integer digit) {
        lock.lock();
        try {
            results.put(position, digit);
        } finally {
            lock.unlock();
        }
    }

    /**
     * Retrieves a copy of the results stored in the table.
     *
     * @return A HashMap containing the positions and corresponding digits.
     *         The returned map is a copy to prevent external modifications.
     */
    public HashMap<Long, Integer> getResults() {
        return new HashMap<>(results);
    }
}
