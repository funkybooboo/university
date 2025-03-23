package org.natestott;

import org.natestott.digitCalculator.DigitCalculator;

/**
 * The DigitWorker class extends Thread and represents a worker thread
 * responsible for processing tasks from a task queue to compute digits
 * of a numerical representation (e.g., digits of Pi).
 *
 * <p>Each DigitWorker retrieves tasks from the TaskQueue, computes the
 * corresponding digit using a DigitCalculator, and stores the result
 * in a ResultTable. The class is designed to run concurrently,
 * allowing multiple instances to operate on different tasks simultaneously.</p>
 *
 * @author Nate Stott
 */
public class DigitWorker extends Thread {

    private final TaskQueue taskQueue;
    private final ResultTable resultTable;
    private final DigitCalculator digitCalculator;

    /**
     * Constructs a DigitWorker with the specified task queue, result table,
     * and digit calculator.
     *
     * @param taskQueue The queue from which tasks will be retrieved.
     * @param resultTable The table where computed results will be stored.
     * @param digitCalculator The calculator used to compute the digits.
     */
    public DigitWorker(TaskQueue taskQueue, ResultTable resultTable, DigitCalculator digitCalculator) {
        this.taskQueue = taskQueue;
        this.resultTable = resultTable;
        this.digitCalculator = digitCalculator;
    }

    /**
     * The main execution method for the thread.
     * It continuously retrieves tasks from the task queue, computes
     * the corresponding digit using the digit calculator, and stores
     * the result in the result table.
     *
     * <p>The method also prints a dot (".") to the console for every
     * 10th digit processed, indicating progress.</p>
     */
    @Override
    public void run() {
        Long task;
        while ((task = taskQueue.getTask()) != null) {
            int digit = digitCalculator.getDigit(task);
            resultTable.putResult(task, digit);

            // Report every 10th digit
            synchronized (System.out) {
                if (task % 10 == 0) {
                    System.out.print(".");
                    System.out.flush();
                }
            }
        }
    }
}
