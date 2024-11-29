package org.example;

import org.example.memory.MemoryCalculator;
import org.example.page.Paging;
import org.example.schedule.Scheduling;

import java.util.Scanner;

public class App {
    public static void main(String[] args) {

        Scanner scanner = new Scanner(System.in);
        String choice = "";

        while (!choice.equals("q")) {
            // Display menu options
            System.out.println("1. FCFS Scheduler");
            System.out.println("2. SJF Scheduler");
            System.out.println("3. SRFT Scheduler");
            System.out.println("4. RR Scheduler");
            System.out.println("5. Priority Scheduler");
            System.out.println("6. Memory Calculator");
            System.out.println("7. FIFO Page Replacement");
            System.out.println("8. MRU Page Replacement");
            System.out.println("9. LRU Page Replacement");
            System.out.println("10. Optimal Page Replacement");
            System.out.println("q to quit");
            System.out.print("Enter: ");
            
            choice = scanner.nextLine();  // Read input safely

            switch (choice) {
                case "1":
                    // FCFS Scheduler (First-Come, First-Served)
                    System.out.println("Running FCFS Scheduler...");
                    Scheduling.calcFCFS();
                    break;
                case "2":
                    // SJF Scheduler (Shortest Job First)
                    System.out.println("Running SJF Scheduler...");
                    Scheduling.calcSJF();
                    break;
                case "3":
                    // SRFT Scheduler (Shortest Remaining Time First)
                    System.out.println("Running SRFT Scheduler...");
                    Scheduling.calcSRTF();
                    break;
                case "4":
                    // RR Scheduler (Round Robin)
                    System.out.println("Running RR Scheduler...");
                    Scheduling.calcRR();
                    break;
                case "5":
                    // Priority Scheduler
                    Scheduling.calcPriority();
                    System.out.println("Running Priority Scheduler...");
                    break;
                case "6":
                    // Memory Calculator
                    System.out.println("Running Memory Calculator...");
                    MemoryCalculator.calc();
                    break;
                case "7":
                    // FIFO Page Replacement
                    System.out.println("Running FIFO Page Replacement...");
                    Paging.calcFIFO();
                    break;
                case "8":
                    // MRU Page Replacement (Most Recently Used)
                    System.out.println("Running MRU Page Replacement...");
                    Paging.calcMRU();
                    break;
                case "9":
                    // LRU Page Replacement (Least Recently Used)
                    System.out.println("Running LRU Page Replacement...");
                    Paging.calcLRU();
                    break;
                case "10":
                    // Optimal Page Replacement
                    System.out.println("Running Optimal Page Replacement...");
                    Paging.calcOPT();
                    break;
                case "q":
                    System.out.println("Quitting...");
                    break;
                default:
                    System.out.println("Invalid option. Please try again.");
            }
        }

        scanner.close();
    }
}
