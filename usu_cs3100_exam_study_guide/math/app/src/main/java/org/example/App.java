package org.example;

import org.example.memory.MemoryCalculator;
import org.example.page.Paging;
import org.example.schedule.Scheduling;

import java.util.Scanner;

public class App {
    public static void main(String[] args) {

        Scanner scanner = new Scanner(System.in);

        // Display menu options starting from 0
        System.out.println("0. FCFS Scheduler");
        System.out.println("1. SJF Scheduler");
        System.out.println("2. SRFT Scheduler");
        System.out.println("3. RR Scheduler");
        System.out.println("4. Priority Scheduler");
        System.out.println("5. Memory Calculator");
        System.out.println("6. FIFO Page Replacement");
        System.out.println("7. MRU Page Replacement");
        System.out.println("8. LRU Page Replacement");
        System.out.println("9. OPT Page Replacement");
        System.out.print("Enter your choice (0-9): ");

        String choice = scanner.nextLine();  // Read input safely

        // Switch case based on user input
        switch (choice) {
            case "0":
                // FCFS Scheduler (First-Come, First-Served)
                System.out.println("Running FCFS Scheduler...");
                Scheduling.calcFCFS();
                break;
            case "1":
                // SJF Scheduler (Shortest Job First)
                System.out.println("Running SJF Scheduler...");
                Scheduling.calcSJF();
                break;
            case "2":
                // SRFT Scheduler (Shortest Remaining Time First)
                System.out.println("Running SRFT Scheduler...");
                Scheduling.calcSRTF();
                break;
            case "3":
                // RR Scheduler (Round Robin)
                System.out.println("Running RR Scheduler...");
                Scheduling.calcRR();
                break;
            case "4":
                // Priority Scheduler
                System.out.println("Running Priority Scheduler...");
                Scheduling.calcPriority();
                break;
            case "5":
                // Memory Calculator
                System.out.println("Running Memory Calculator...");
                MemoryCalculator.calc();
                break;
            case "6":
                // FIFO Page Replacement
                System.out.println("Running FIFO Page Replacement...");
                Paging.calcFIFO();
                break;
            case "7":
                // MRU Page Replacement (Most Recently Used)
                System.out.println("Running MRU Page Replacement...");
                Paging.calcMRU();
                break;
            case "8":
                // LRU Page Replacement (Least Recently Used)
                System.out.println("Running LRU Page Replacement...");
                Paging.calcLRU();
                break;
            case "9":
                // Optimal Page Replacement
                System.out.println("Running OPT Page Replacement...");
                Paging.calcOPT();
                break;
            default:
                System.out.println("Invalid option. Please choose a valid option (0-9).");
        }

        scanner.close();
    }
}
