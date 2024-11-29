package org.example.page;

import java.util.Scanner;

public class Paging {
    
    // Helper method to get page reference sequence and frame count from user input
    private static int[] getUserInputSequence() {
        Scanner scanner = new Scanner(System.in);
        System.out.println("Enter the page reference sequence (space-separated):");
        String input = scanner.nextLine();
        String[] parts = input.split("\\s+");
        int[] sequence = new int[parts.length];
        for (int i = 0; i < parts.length; i++) {
            sequence[i] = Integer.parseInt(parts[i].trim());
        }
        return sequence;
    }

    private static int getUserInputFrameCount() {
        Scanner scanner = new Scanner(System.in);
        System.out.println("Enter the number of memory frames:");
        return scanner.nextInt();
    }

    // Running the FIFO algorithm
    public static void calcFIFO() {
        int[] sequence = getUserInputSequence();
        int frameCount = getUserInputFrameCount();

        FIFO fifo = new FIFO(sequence, frameCount, new int[frameCount + 1]);
        fifo.run();
    }

    // Running the LRU algorithm
    public static void calcLRU() {
        int[] sequence = getUserInputSequence();
        int frameCount = getUserInputFrameCount();

        LRU lru = new LRU(sequence, frameCount, new int[frameCount + 1]);
        lru.run();
    }

    // Running the MRU algorithm
    public static void calcMRU() {
        int[] sequence = getUserInputSequence();
        int frameCount = getUserInputFrameCount();

        MRU mru = new MRU(sequence, frameCount, new int[frameCount + 1]);
        mru.run();
    }

    // Running the OPT algorithm
    public static void calcOPT() {
        int[] sequence = getUserInputSequence();
        int frameCount = getUserInputFrameCount();

        OPT opt = new OPT(sequence, frameCount, new int[frameCount + 1]);
        opt.run();
    }
}
