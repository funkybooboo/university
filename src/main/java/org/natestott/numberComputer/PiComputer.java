package org.natestott.numberComputer;

import org.natestott.digitCalculator.Bpp;
import org.natestott.digitCalculator.DigitCalculator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

public class PiComputer implements NumberComputer {

    public void computeNumberAndPrint() {
        computeNumberAndPrint(1000);
    }

    public void computeNumberAndPrint(final int num_digits) {
        TaskQueue taskQueue = new TaskQueue();
        ResultTable resultTable = new ResultTable();
        DigitCalculator digitCalculator = new Bpp();
        List<Long> tasks = new ArrayList<>();

        for (long i = 0; i < num_digits; i++) {
            tasks.add(i);
        }
        Collections.shuffle(tasks); // Randomize the task order
        for (Long task : tasks) {
            taskQueue.addTask(task);
        }

        // Create worker threads based on the number of available processors
        int numCores = Runtime.getRuntime().availableProcessors();
        Thread[] workers = new Thread[numCores];

        long startTime = System.currentTimeMillis();
        for (int i = 0; i < numCores; i++) {
            workers[i] = new DigitWorker(taskQueue, resultTable, digitCalculator);
            workers[i].start();
        }

        // Wait for all threads to finish
        for (Thread worker : workers) {
            try {
                worker.join();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }

        long endTime = System.currentTimeMillis();
        long elapsedTime = endTime - startTime;

        // Collect results and sort them
        HashMap<Long, Integer> results = resultTable.getResults();
        StringBuilder piDigits = new StringBuilder("3.");
        for (long i = 0; i < num_digits; i++) {
            piDigits.append(results.get(i));
        }

        System.out.println("\n"+piDigits);
        System.out.println("Pi Computation took " + elapsedTime + " ms");
    }
}
