package org.example.memory;

import java.util.Scanner;

public class MemoryCalculator {
    public static void calc() {
        // Create scanner to read user input
        Scanner scanner = new Scanner(System.in);

        // Prompt the user for process size and page size
        System.out.print("Enter the process size in bytes: ");
        int processSize = scanner.nextInt();

        System.out.print("Enter the page size in bytes: ");
        int pageSize = scanner.nextInt();

        // Calculate the number of pages required (using integer arithmetic for ceiling)
        int numberOfPages = (processSize + pageSize - 1) / pageSize;

        // Calculate total space (number of pages * page size)
        int totalSpace = numberOfPages * pageSize;

        // Calculate internal fragmentation (unused space in last page)
        int internalFragmentation = totalSpace - processSize;

        // Calculate number of bits required for the page number
        int bitsForPageNumber = (int) Math.ceil(Math.log(numberOfPages) / Math.log(2));

        // Calculate offset bits (bits needed to address a location within a page)
        int offsetBits = (int) Math.ceil(Math.log(pageSize) / Math.log(2));

        // Calculate the total number of bits required for the logical address
        int totalBitsForLogicalAddress = bitsForPageNumber + offsetBits;

        // Output all the calculated values
        System.out.println("\n--- Memory Calculation Results ---");
        System.out.println("Process Size: " + processSize + " bytes");
        System.out.println("Page Size: " + pageSize + " bytes");
        System.out.println("Number of Pages: " + numberOfPages);
        System.out.println("Total Space: " + totalSpace + " bytes");
        System.out.println("Internal Fragmentation: " + internalFragmentation + " bytes");
        System.out.println("Bits for Page Number: " + bitsForPageNumber + " bits");
        System.out.println("Offset Bits: " + offsetBits + " bits");
        System.out.println("Total Bits for Logical Address: " + totalBitsForLogicalAddress + " bits");

        // Close the scanner
        scanner.close();
    }
}
