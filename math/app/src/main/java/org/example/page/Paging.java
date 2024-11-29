package org.example.page;

/**
 * The Assign6 class simulates page replacement algorithms (FIFO, LRU, and MRU)
 * and evaluates their performance through a series of simulations.
 * It tracks page faults for each algorithm and performs a comparison to detect 
 * Belady's Anomaly, where the page fault count increases with more frames.
 */
public class Paging {
    
    public static void calcFIFO() {
        int[] pageFaults = new int[2];

        int[] sequence = new int[1]; // TODO ask them for the sequence and frame count

        FIFO fifo = new FIFO(sequence, 1, pageFaults);
        fifo.run();
    }

    public static void calcLRU() {
        int[] pageFaults = new int[2];

        int[] sequence = new int[1]; // TODO ask them for the sequence and frame count

        LRU lru = new LRU(sequence, 1, pageFaults);
        lru.run();
    }

    public static void calcMRU() {
        int[] pageFaults = new int[2];

        int[] sequence = new int[1]; // TODO ask them for the sequence and frame count

        MRU mru = new MRU(sequence, 1, pageFaults);
        mru.run();
    }

    public static void calcOPT() {
        int[] pageFaults = new int[2];

        int[] sequence = new int[1]; // TODO ask them for the sequence and frame count

        OPT opt = new OPT(sequence, 1, pageFaults);
        opt.run();
    }
}
