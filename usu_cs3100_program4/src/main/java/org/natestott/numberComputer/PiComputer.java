package org.natestott.numberComputer;

import org.natestott.DigitWorker;
import org.natestott.ResultTable;
import org.natestott.TaskQueue;
import org.natestott.digitCalculator.Bpp;
import org.natestott.digitCalculator.DigitCalculator;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

/**
 * The PiComputer class implements the NumberComputer interface and provides
 * functionality to compute the digits of Pi using a multi-threaded approach.
 * It utilizes a task queue to distribute work among available processor cores,
 * leveraging the Bpp digit calculation method.
 *
 * <p>This class is part of the org.natestott.numberComputer package.</p>
 *
 * @author Nate Stott
 */
public class PiComputer implements NumberComputer {

    /**
     * Computes the first 1000 digits of Pi and prints them.
     * This method is a convenience method that calls
     * {@link #computeNumberAndPrint(int)} with a default of 1000 digits.
     */
    public void computeNumberAndPrint() {
        computeNumberAndPrint(1000);
    }

    /**
     * Computes the specified number of digits of Pi and prints the result.
     *
     * <p>This method initializes a task queue, distributes tasks across
     * multiple threads, and collects the computed digits into a result table.</p>
     *
     * @param num_digits The number of digits of Pi to compute.
     */
    public void computeNumberAndPrint(final int num_digits) {
        TaskQueue taskQueue = new TaskQueue();
        ResultTable resultTable = new ResultTable();
        DigitCalculator digitCalculator = new Bpp();
        List<Long> tasks = new ArrayList<>();

        for (long task = 1; task <= num_digits; task++) {
            tasks.add(task);
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
        for (long task = 1; task <= num_digits; task++) {
            piDigits.append(results.get(task));
        }

        System.out.println("\n" + piDigits);
        System.out.println("Pi Computation took " + elapsedTime + " ms");

        // Uncomment if you'd like to check if the digits are valid
        // test(piDigits.toString(), num_digits);
    }

    /**
     * Tests the computed digits of Pi against a known file of Pi digits.
     *
     * <p>This method compares the computed digits to the digits stored
     * in a file and reports any differences.</p>
     *
     * @param piDigits  The computed digits of Pi as a String.
     * @param num_digits The number of digits that were computed.
     */
    private void test(String piDigits, int num_digits) {
        File file = new File("src/main/java/org/natestott/numberComputer/pi.txt");
        StringBuilder piFromFile = new StringBuilder();

        try (Scanner scanner = new Scanner(file)) {
            if (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                piFromFile.append(line, 0, Math.min(num_digits + 2, line.length()));
            }
        } catch (FileNotFoundException e) {
            System.out.println("File not found: " + e.getMessage());
            return;
        }

        if (piFromFile.length() == num_digits + 2 && piDigits.length() == num_digits + 2) {
            System.out.println("Valid length");
        } else {
            System.out.println("Invalid length");
        }

        if (piFromFile.toString().equals(piDigits)) {
            System.out.println("The input matches the first " + num_digits + " digits of Pi.");
        } else {
            System.out.println("The input does not match the first " + num_digits + " digits of Pi.");
            for (int i = 0; i < Math.min(piFromFile.length(), piDigits.length()); i++) {
                if (piFromFile.charAt(i) != piDigits.charAt(i)) {
                    System.out.println("Difference at index " + i + ": file='" + piFromFile.charAt(i) + "', input='" + piDigits.charAt(i) + "'");
                }
            }
        }
    }
}
