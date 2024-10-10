package org.natestott.numberComputer;

import org.natestott.digitCalculator.DigitCalculator;

class DigitWorker extends Thread {
    private final TaskQueue taskQueue;
    private final ResultTable resultTable;
    private final DigitCalculator digitCalculator;

    public DigitWorker(TaskQueue taskQueue, ResultTable resultTable, DigitCalculator digitCalculator) {
        this.taskQueue = taskQueue;
        this.resultTable = resultTable;
        this.digitCalculator = digitCalculator;
    }

    @Override
    public void run() {
        Long task;
        while ((task = taskQueue.getTask()) != null) {
            int digit = digitCalculator.getDecimal(task);
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