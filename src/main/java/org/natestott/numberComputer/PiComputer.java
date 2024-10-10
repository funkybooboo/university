package org.natestott.numberComputer;

import org.natestott.DigitWorker;
import org.natestott.ResultTable;
import org.natestott.TaskQueue;
import org.natestott.digitCalculator.Bpp;
import org.natestott.digitCalculator.DigitCalculator;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

public class PiComputer implements NumberComputer {

    public void computeNumberAndPrint() {
        computeNumberAndPrint(1000);
    }

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

        System.out.println("\n"+piDigits);
        System.out.println("Pi Computation took " + elapsedTime + " ms");

        // uncomment if you'd like to check if the digits are valid
        // test(piDigits.toString(), num_digits);
    }

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
            System.out.println("valid length");
        }
        else {
            System.out.println("invalid length");
        }

        if (piFromFile.toString().equals(piDigits)) {
            System.out.println("The input matches the first 1000 digits of pi.");
        }
        else {
            System.out.println("The input does not match the first 1000 digits of pi.");
            for (int i = 0; i < Math.min(piFromFile.length(), piDigits.length()); i++) {
                if (piFromFile.charAt(i) != piDigits.charAt(i)) {
                    System.out.println("Difference at index " + i + ": file='" + piFromFile.charAt(i) + "', input='" + piDigits.charAt(i) + "'");
                }
            }
        }
    }
}
