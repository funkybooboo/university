package org.example.schedule;

import java.util.LinkedList;
import java.util.Queue;
import java.util.Scanner;

public class Scheduling {
    /**
     * First-Come, First Served
     */
    public static void calcFCFS() {
        final int cpuCount = 1;
        Platform platform = new Platform(cpuCount);
        Queue<Process> processes = new LinkedList<>();
        Scanner scanner = new Scanner(System.in);

        // Ask the user how many processes they want to enter
        System.out.print("Enter the number of processes: ");
        int numProcesses = scanner.nextInt();
        scanner.nextLine(); // Consume the newline character after the integer input

        // Arrays to store the user input
        int[] startTimes = new int[numProcesses];
        int[] burstTimes = new int[numProcesses];
        int[] totalTimes = new int[numProcesses];

        // Ask the user to enter start times as a space-separated list
        System.out.print("Enter the start times for the processes (space-separated): ");
        String startTimesInput = scanner.nextLine();
        String[] startTimeStrings = startTimesInput.split("\\s+");

        // Ensure that the number of start times matches the number of processes
        if (startTimeStrings.length != numProcesses) {
            System.out.println("Error: Number of start times does not match the number of processes.");
            return;
        }

        // Parse the start times into the array
        for (int i = 0; i < numProcesses; i++) {
            startTimes[i] = Integer.parseInt(startTimeStrings[i]);
        }

        // Ask the user to enter burst times as a space-separated list
        System.out.print("Enter the burst times for the processes (space-separated): ");
        String burstTimesInput = scanner.nextLine();
        String[] burstTimeStrings = burstTimesInput.split("\\s+");

        // Ensure that the number of burst times matches the number of processes
        if (burstTimeStrings.length != numProcesses) {
            System.out.println("Error: Number of burst times does not match the number of processes.");
            return;
        }

        // Parse the burst times into the array
        for (int i = 0; i < numProcesses; i++) {
            burstTimes[i] = Integer.parseInt(burstTimeStrings[i]);
        }

        // Ask the user to enter total times (completion times) as a space-separated list
        System.out.print("Enter the total times (completion times) for the processes (space-separated): ");
        String totalTimesInput = scanner.nextLine();
        String[] totalTimeStrings = totalTimesInput.split("\\s+");

        // Ensure that the number of total times matches the number of processes
        if (totalTimeStrings.length != numProcesses) {
            System.out.println("Error: Number of total times does not match the number of processes.");
            return;
        }

        // Parse the total times into the array
        for (int i = 0; i < numProcesses; i++) {
            totalTimes[i] = Integer.parseInt(totalTimeStrings[i]);
        }

        // Create processes with the user-provided data
        for (int i = 0; i < numProcesses; i++) {
            String processName = "P" + (i + 1); // Naming convention like P1, P2, P3, etc.
            processes.add(new Process(processName, startTimes[i], burstTimes[i], totalTimes[i]));
        }

        // Display starting message
        System.out.println("\nStarting First Come, First Served CPU scheduling simulation");

        // Simulate the scheduling
        Scheduler scheduler = new SchedulerFCFS(platform);
        platform.simulate(scheduler, processes);

        // Output the results
        System.out.printf("Number of context switches: %d\n", scheduler.getNumberOfContextSwitches());
        System.out.println("FCFS CPU scheduling simulation complete");

        // Close the scanner to avoid resource leaks
        scanner.close();
    }

    /**
     * Shortest Job First
     */
    public static void calcSJF() {
        final int cpuCount = 1;
        Platform platform = new Platform(cpuCount);
        Queue<Process> processes = new LinkedList<>();
        Scanner scanner = new Scanner(System.in);

        // Ask the user how many processes they want to enter
        System.out.print("Enter the number of processes: ");
        int numProcesses = scanner.nextInt();
        scanner.nextLine(); // Consume the newline character after the integer input

        // Arrays to store the user input
        int[] startTimes = new int[numProcesses];
        int[] burstTimes = new int[numProcesses];
        int[] totalTimes = new int[numProcesses];

        // Ask the user to enter start times as a space-separated list
        System.out.print("Enter the start times for the processes (space-separated): ");
        String startTimesInput = scanner.nextLine();
        String[] startTimeStrings = startTimesInput.split("\\s+");

        // Ensure that the number of start times matches the number of processes
        if (startTimeStrings.length != numProcesses) {
            System.out.println("Error: Number of start times does not match the number of processes.");
            return;
        }

        // Parse the start times into the array
        for (int i = 0; i < numProcesses; i++) {
            startTimes[i] = Integer.parseInt(startTimeStrings[i]);
        }

        // Ask the user to enter burst times as a space-separated list
        System.out.print("Enter the burst times for the processes (space-separated): ");
        String burstTimesInput = scanner.nextLine();
        String[] burstTimeStrings = burstTimesInput.split("\\s+");

        // Ensure that the number of burst times matches the number of processes
        if (burstTimeStrings.length != numProcesses) {
            System.out.println("Error: Number of burst times does not match the number of processes.");
            return;
        }

        // Parse the burst times into the array
        for (int i = 0; i < numProcesses; i++) {
            burstTimes[i] = Integer.parseInt(burstTimeStrings[i]);
        }

        // Ask the user to enter total times (completion times) as a space-separated list
        System.out.print("Enter the total times (completion times) for the processes (space-separated): ");
        String totalTimesInput = scanner.nextLine();
        String[] totalTimeStrings = totalTimesInput.split("\\s+");

        // Ensure that the number of total times matches the number of processes
        if (totalTimeStrings.length != numProcesses) {
            System.out.println("Error: Number of total times does not match the number of processes.");
            return;
        }

        // Parse the total times into the array
        for (int i = 0; i < numProcesses; i++) {
            totalTimes[i] = Integer.parseInt(totalTimeStrings[i]);
        }

        // Create processes with the user-provided data
        for (int i = 0; i < numProcesses; i++) {
            String processName = "P" + (i + 1); // Naming convention like P1, P2, P3, etc.
            processes.add(new Process(processName, startTimes[i], burstTimes[i], totalTimes[i]));
        }

        // Display starting message
        System.out.println("\nStarting Shortest Job First CPU scheduling simulation");

        // Simulate the scheduling
        Scheduler scheduler = new SchedulerSJF(platform);
        platform.simulate(scheduler, processes);

        // Output the results
        System.out.printf("Number of context switches: %d\n", scheduler.getNumberOfContextSwitches());
        System.out.println("SJF CPU scheduling simulation complete");

        // Close the scanner to avoid resource leaks
        scanner.close();
    }

    /**
     * Shortest Remaining Time First
     */
    public static void calcSRTF() {
        final int cpuCount = 1;
        Platform platform = new Platform(cpuCount);
        Queue<Process> processes = new LinkedList<>();
        Scanner scanner = new Scanner(System.in);

        // Ask the user how many processes they want to enter
        System.out.print("Enter the number of processes: ");
        int numProcesses = scanner.nextInt();
        scanner.nextLine(); // Consume the newline character after the integer input

        // Arrays to store the user input
        int[] startTimes = new int[numProcesses];
        int[] burstTimes = new int[numProcesses];
        int[] totalTimes = new int[numProcesses];

        // Ask the user to enter start times as a space-separated list
        System.out.print("Enter the start times for the processes (space-separated): ");
        String startTimesInput = scanner.nextLine();
        String[] startTimeStrings = startTimesInput.split("\\s+");

        // Ensure that the number of start times matches the number of processes
        if (startTimeStrings.length != numProcesses) {
            System.out.println("Error: Number of start times does not match the number of processes.");
            return;
        }

        // Parse the start times into the array
        for (int i = 0; i < numProcesses; i++) {
            startTimes[i] = Integer.parseInt(startTimeStrings[i]);
        }

        // Ask the user to enter burst times as a space-separated list
        System.out.print("Enter the burst times for the processes (space-separated): ");
        String burstTimesInput = scanner.nextLine();
        String[] burstTimeStrings = burstTimesInput.split("\\s+");

        // Ensure that the number of burst times matches the number of processes
        if (burstTimeStrings.length != numProcesses) {
            System.out.println("Error: Number of burst times does not match the number of processes.");
            return;
        }

        // Parse the burst times into the array
        for (int i = 0; i < numProcesses; i++) {
            burstTimes[i] = Integer.parseInt(burstTimeStrings[i]);
        }

        // Ask the user to enter total times (completion times) as a space-separated list
        System.out.print("Enter the total times (completion times) for the processes (space-separated): ");
        String totalTimesInput = scanner.nextLine();
        String[] totalTimeStrings = totalTimesInput.split("\\s+");

        // Ensure that the number of total times matches the number of processes
        if (totalTimeStrings.length != numProcesses) {
            System.out.println("Error: Number of total times does not match the number of processes.");
            return;
        }

        // Parse the total times into the array
        for (int i = 0; i < numProcesses; i++) {
            totalTimes[i] = Integer.parseInt(totalTimeStrings[i]);
        }

        // Create processes with the user-provided data
        for (int i = 0; i < numProcesses; i++) {
            String processName = "P" + (i + 1); // Naming convention like P1, P2, P3, etc.
            processes.add(new Process(processName, startTimes[i], burstTimes[i], totalTimes[i]));
        }

        // Display starting message
        System.out.println("\nStarting Shortest Remaining Time First CPU scheduling simulation");

        // Simulate the scheduling
        Scheduler scheduler = new SchedulerSRTF(platform);
        platform.simulate(scheduler, processes);

        // Output the results
        System.out.printf("Number of context switches: %d\n", scheduler.getNumberOfContextSwitches());
        System.out.println("SRTF CPU scheduling simulation complete");

        // Close the scanner to avoid resource leaks
        scanner.close();
    }

    /**
     * Priority
     */
    public static void calcPriority() {
        final int cpuCount = 1;
        Platform platform = new Platform(cpuCount);
        Queue<Process> processes = new LinkedList<>();
        Scanner scanner = new Scanner(System.in);

        // Ask the user how many processes they want to enter
        System.out.print("Enter the number of processes: ");
        int numProcesses = scanner.nextInt();
        scanner.nextLine(); // Consume the newline character after the integer input

        // Arrays to store the user input
        int[] startTimes = new int[numProcesses];
        int[] burstTimes = new int[numProcesses];
        int[] totalTimes = new int[numProcesses];
        int[] priorities = new int[numProcesses];

        // Ask the user to enter start times as a space-separated list
        System.out.print("Enter the start times for the processes (space-separated): ");
        String startTimesInput = scanner.nextLine();
        String[] startTimeStrings = startTimesInput.split("\\s+");

        // Ensure that the number of start times matches the number of processes
        if (startTimeStrings.length != numProcesses) {
            System.out.println("Error: Number of start times does not match the number of processes.");
            return;
        }

        // Parse the start times into the array
        for (int i = 0; i < numProcesses; i++) {
            startTimes[i] = Integer.parseInt(startTimeStrings[i]);
        }

        // Ask the user to enter burst times as a space-separated list
        System.out.print("Enter the burst times for the processes (space-separated): ");
        String burstTimesInput = scanner.nextLine();
        String[] burstTimeStrings = burstTimesInput.split("\\s+");

        // Ensure that the number of burst times matches the number of processes
        if (burstTimeStrings.length != numProcesses) {
            System.out.println("Error: Number of burst times does not match the number of processes.");
            return;
        }

        // Parse the burst times into the array
        for (int i = 0; i < numProcesses; i++) {
            burstTimes[i] = Integer.parseInt(burstTimeStrings[i]);
        }

        // Ask the user to enter total times (completion times) as a space-separated list
        System.out.print("Enter the total times (completion times) for the processes (space-separated): ");
        String totalTimesInput = scanner.nextLine();
        String[] totalTimeStrings = totalTimesInput.split("\\s+");

        // Ensure that the number of total times matches the number of processes
        if (totalTimeStrings.length != numProcesses) {
            System.out.println("Error: Number of total times does not match the number of processes.");
            return;
        }

        // Parse the total times into the array
        for (int i = 0; i < numProcesses; i++) {
            totalTimes[i] = Integer.parseInt(totalTimeStrings[i]);
        }

        // Ask the user to enter priorities as a space-separated list
        System.out.print("Enter the priorities for the processes (space-separated): ");
        String prioritiesInput = scanner.nextLine();
        String[] priorityStrings = prioritiesInput.split("\\s+");

        // Ensure that the number of priorities matches the number of processes
        if (priorityStrings.length != numProcesses) {
            System.out.println("Error: Number of priorities does not match the number of processes.");
            return;
        }

        // Parse the priorities into the array
        for (int i = 0; i < numProcesses; i++) {
            priorities[i] = Integer.parseInt(priorityStrings[i]);
        }

        // Create processes with the user-provided data
        for (int i = 0; i < numProcesses; i++) {
            String processName = "P" + (i + 1); // Naming convention like P1, P2, P3, etc.
            processes.add(new Process(processName, startTimes[i], burstTimes[i], totalTimes[i], priorities[i]));
        }

        // Display starting message
        System.out.println("\nStarting Priority CPU scheduling simulation");

        // Simulate the scheduling
        Scheduler scheduler = new SchedulerPriority(platform);
        platform.simulate(scheduler, processes);

        // Output the results
        System.out.printf("Number of context switches: %d\n", scheduler.getNumberOfContextSwitches());
        System.out.println("Priority CPU scheduling simulation complete");

        // Close the scanner to avoid resource leaks
        scanner.close();
    }

    /**
     * Round Robin
     */
    public static void calcRR() {
        final int cpuCount = 1;
        Platform platform = new Platform(cpuCount);
        Queue<Process> processes = new LinkedList<>();
        Scanner scanner = new Scanner(System.in);

        // Ask the user how many processes they want to enter
        System.out.print("Enter the number of processes: ");
        int numProcesses = scanner.nextInt();
        scanner.nextLine(); // Consume the newline character after the integer input

        // Arrays to store the user input
        int[] startTimes = new int[numProcesses];
        int[] burstTimes = new int[numProcesses];
        int[] totalTimes = new int[numProcesses];

        // Ask the user to enter start times as a space-separated list
        System.out.print("Enter the start times for the processes (space-separated): ");
        String startTimesInput = scanner.nextLine();
        String[] startTimeStrings = startTimesInput.split("\\s+");

        // Ensure that the number of start times matches the number of processes
        if (startTimeStrings.length != numProcesses) {
            System.out.println("Error: Number of start times does not match the number of processes.");
            return;
        }

        // Parse the start times into the array
        for (int i = 0; i < numProcesses; i++) {
            startTimes[i] = Integer.parseInt(startTimeStrings[i]);
        }

        // Ask the user to enter burst times as a space-separated list
        System.out.print("Enter the burst times for the processes (space-separated): ");
        String burstTimesInput = scanner.nextLine();
        String[] burstTimeStrings = burstTimesInput.split("\\s+");

        // Ensure that the number of burst times matches the number of processes
        if (burstTimeStrings.length != numProcesses) {
            System.out.println("Error: Number of burst times does not match the number of processes.");
            return;
        }

        // Parse the burst times into the array
        for (int i = 0; i < numProcesses; i++) {
            burstTimes[i] = Integer.parseInt(burstTimeStrings[i]);
        }

        // Ask the user to enter total times (completion times) as a space-separated list
        System.out.print("Enter the total times (completion times) for the processes (space-separated): ");
        String totalTimesInput = scanner.nextLine();
        String[] totalTimeStrings = totalTimesInput.split("\\s+");

        // Ensure that the number of total times matches the number of processes
        if (totalTimeStrings.length != numProcesses) {
            System.out.println("Error: Number of total times does not match the number of processes.");
            return;
        }

        // Parse the total times into the array
        for (int i = 0; i < numProcesses; i++) {
            totalTimes[i] = Integer.parseInt(totalTimeStrings[i]);
        }

        // Ask for the time quantum
        System.out.print("Enter the time quantum for Round Robin scheduling: ");
        int timeQuantum = scanner.nextInt();

        // Create processes with the user-provided data
        for (int i = 0; i < numProcesses; i++) {
            String processName = "P" + (i + 1); // Naming convention like P1, P2, P3, etc.
            processes.add(new Process(processName, startTimes[i], burstTimes[i], totalTimes[i]));
        }

        // Display starting message
        System.out.println("\nStarting Round Robin CPU scheduling simulation");

        // Simulate the scheduling
        Scheduler scheduler = new SchedulerRR(platform, timeQuantum);
        platform.simulate(scheduler, processes);

        // Output the results
        System.out.printf("Number of context switches: %d\n", scheduler.getNumberOfContextSwitches());
        System.out.println("RR CPU scheduling simulation complete");

        // Close the scanner to avoid resource leaks
        scanner.close();
    }
}
