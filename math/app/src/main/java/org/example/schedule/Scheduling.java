package org.example.schedule;/* Add a file (and path) as a commandline argument to process an input file. 
 * Otherwise, hardcoded default values will apply. The default values do make
 * it easier to see how the code works. 
*/

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

        // Ask the user for the start times
        System.out.println("Enter the start times for the processes:");
        for (int i = 0; i < numProcesses; i++) {
            System.out.printf("Enter start time for Process %d: ", i + 1);
            startTimes[i] = scanner.nextInt();
        }

        // Ask the user for the burst times
        System.out.println("Enter the burst times for the processes:");
        for (int i = 0; i < numProcesses; i++) {
            System.out.printf("Enter burst time for Process %d: ", i + 1);
            burstTimes[i] = scanner.nextInt();
        }

        // Ask the user for the total times (completion times)
        System.out.println("Enter the total times (completion times) for the processes:");
        for (int i = 0; i < numProcesses; i++) {
            System.out.printf("Enter total time for Process %d: ", i + 1);
            totalTimes[i] = scanner.nextInt();
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

        // Ask the user for the start times
        System.out.println("Enter the start times for the processes:");
        for (int i = 0; i < numProcesses; i++) {
            System.out.printf("Enter start time for Process %d: ", i + 1);
            startTimes[i] = scanner.nextInt();
        }

        // Ask the user for the burst times
        System.out.println("Enter the burst times for the processes:");
        for (int i = 0; i < numProcesses; i++) {
            System.out.printf("Enter burst time for Process %d: ", i + 1);
            burstTimes[i] = scanner.nextInt();
        }

        // Ask the user for the total times (completion times)
        System.out.println("Enter the total times (completion times) for the processes:");
        for (int i = 0; i < numProcesses; i++) {
            System.out.printf("Enter total time for Process %d: ", i + 1);
            totalTimes[i] = scanner.nextInt();
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

        // Ask the user for the start times
        System.out.println("Enter the start times for the processes:");
        for (int i = 0; i < numProcesses; i++) {
            System.out.printf("Enter start time for Process %d: ", i + 1);
            startTimes[i] = scanner.nextInt();
        }

        // Ask the user for the burst times
        System.out.println("Enter the burst times for the processes:");
        for (int i = 0; i < numProcesses; i++) {
            System.out.printf("Enter burst time for Process %d: ", i + 1);
            burstTimes[i] = scanner.nextInt();
        }

        // Ask the user for the total times (completion times)
        System.out.println("Enter the total times (completion times) for the processes:");
        for (int i = 0; i < numProcesses; i++) {
            System.out.printf("Enter total time for Process %d: ", i + 1);
            totalTimes[i] = scanner.nextInt();
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

        // Ask the user for the start times
        System.out.println("Enter the start times for the processes:");
        for (int i = 0; i < numProcesses; i++) {
            System.out.printf("Enter start time for Process %d: ", i + 1);
            startTimes[i] = scanner.nextInt();
        }

        // Ask the user for the burst times
        System.out.println("Enter the burst times for the processes:");
        for (int i = 0; i < numProcesses; i++) {
            System.out.printf("Enter burst time for Process %d: ", i + 1);
            burstTimes[i] = scanner.nextInt();
        }

        // Ask the user for the total times (completion times)
        System.out.println("Enter the total times (completion times) for the processes:");
        for (int i = 0; i < numProcesses; i++) {
            System.out.printf("Enter total time for Process %d: ", i + 1);
            totalTimes[i] = scanner.nextInt();
        }

        // Ask the user for the priority of each process
        System.out.println("Enter the priorities for the processes:");
        for (int i = 0; i < numProcesses; i++) {
            System.out.printf("Enter priority for Process %d: ", i + 1);
            priorities[i] = scanner.nextInt();
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

        // Ask the user for the start times
        System.out.println("Enter the start times for the processes:");
        for (int i = 0; i < numProcesses; i++) {
            System.out.printf("Enter start time for Process %d: ", i + 1);
            startTimes[i] = scanner.nextInt();
        }

        // Ask the user for the burst times
        System.out.println("Enter the burst times for the processes:");
        for (int i = 0; i < numProcesses; i++) {
            System.out.printf("Enter burst time for Process %d: ", i + 1);
            burstTimes[i] = scanner.nextInt();
        }

        // Ask the user for the total times (completion times)
        System.out.println("Enter the total times (completion times) for the processes:");
        for (int i = 0; i < numProcesses; i++) {
            System.out.printf("Enter total time for Process %d: ", i + 1);
            totalTimes[i] = scanner.nextInt();
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
