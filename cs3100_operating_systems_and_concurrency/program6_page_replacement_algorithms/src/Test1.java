import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

/**
 * The Assign6 class simulates page replacement algorithms (FIFO, LRU, and MRU)
 * and evaluates their performance through a series of simulations.
 * It tracks page faults for each algorithm and performs a comparison to detect 
 * Belady's Anomaly, where the page fault count increases with more frames.
 */
public class Test1 {

    // Anomaly detected in simulation #340 - 16 PF's @   6 frames vs. 17 PF's @   7 frames (Δ1)
    private static final int[] anomaly340 = {11, 2, 5, 1, 11, 2, 10, 3, 5, 0, 13, 2, 10, 5, 9, 11, 7, 7, 2, 14, 5, 7, 12, 10, 9};
    // Anomaly detected in simulation #8505 - 17 PF's @   6 frames vs. 18 PF's @   7 frames (Δ1)
    private static final int[] anomaly8505 = {13, 3, 7, 11, 1, 7, 3, 6, 9, 4, 3, 12, 1, 7, 14, 0, 11, 3, 7, 0, 8, 1, 11, 12, 14};
    // Anomaly detected in simulation #9044 - 15 PF's @   5 frames vs. 16 PF's @   6 frames (Δ1)
    private static final int[] anomaly9044 = {1, 9, 4, 5, 12, 8, 1, 10, 4, 8, 9, 14, 2, 9, 3, 14, 4, 5, 14, 6, 14, 5, 5, 2, 2};
    // Anomaly detected in simulation #14774 - 17 PF's @   4 frames vs. 18 PF's @   5 frames (Δ1)
    private static final int[] anomaly14774 = {1, 6, 9, 11, 2, 2, 4, 6, 0, 9, 12, 6, 9, 0, 14, 1, 8, 1, 12, 3, 8, 6, 8, 11, 14};
    // Anomaly detected in simulation #16860 - 16 PF's @   4 frames vs. 17 PF's @   5 frames (Δ1)
    private static final int[] anomaly16860 = {13, 0, 4, 5, 13, 5, 7, 13, 1, 4, 2, 13, 4, 9, 11, 5, 9, 2, 1, 11, 13, 1, 11, 10, 4};
    // Anomaly detected in simulation #16877 - 15 PF's @   5 frames vs. 16 PF's @   6 frames (Δ1)
    private static final int[] anomaly16877 = {5, 3, 14, 8, 8, 11, 1, 10, 3, 14, 14, 1, 7, 0, 0, 13, 14, 3, 4, 13, 12, 7, 3, 0, 0};
    // Anomaly detected in simulation #20596 - 15 PF's @   5 frames vs. 16 PF's @   6 frames (Δ1)
    private static final int[] anomaly20596 = {11, 10, 3, 14, 12, 12, 3, 7, 9, 10, 3, 10, 6, 2, 3, 10, 10, 11, 6, 13, 1, 2, 2, 6, 4};
    // Anomaly detected in simulation #21867 - 16 PF's @   5 frames vs. 17 PF's @   6 frames (Δ1)
    private static final int[] anomaly21867 = {3, 11, 14, 10, 1, 1, 5, 4, 14, 7, 14, 4, 11, 10, 0, 14, 1, 8, 3, 1, 1, 10, 3, 11, 0};
    // Anomaly detected in simulation #22361 - 18 PF's @   6 frames vs. 19 PF's @   7 frames (Δ1)
    private static final int[] anomaly22361 = {9, 8, 2, 1, 10, 0, 5, 9, 12, 8, 14, 3, 9, 8, 5, 6, 11, 2, 6, 5, 4, 6, 3, 9, 2};
    // Anomaly detected in simulation #23154 - 17 PF's @   6 frames vs. 18 PF's @   7 frames (Δ1)
    private static final int[] anomaly23154 = {13, 0, 4, 7, 4, 9, 13, 9, 3, 10, 8, 8, 0, 4, 2, 0, 4, 6, 5, 9, 14, 5, 7, 2, 13};
    // Anomaly detected in simulation #23953 - 17 PF's @   5 frames vs. 18 PF's @   6 frames (Δ1)
    private static final int[] anomaly23953 = {9, 5, 10, 6, 8, 1, 3, 6, 1, 3, 5, 11, 8, 10, 2, 5, 6, 12, 1, 12, 10, 7, 2, 5, 11};
    // Anomaly detected in simulation #84391 - 18 PF's @   4 frames vs. 19 PF's @   5 frames (Δ1)
    private static final int[] anomaly84391 = {13, 14, 6, 0, 3, 2, 9, 0, 6, 11, 0, 10, 10, 0, 6, 3, 13, 7, 10, 11, 8, 8, 4, 1, 1};
    // There are no anomalies in these sequences
    private static final int[] noAnomaly0 = {1, 7, 14, 11, 1, 13, 11, 12, 0, 5, 9, 3, 2, 0, 2, 12, 10, 2, 11, 6, 2, 7, 14, 6, 14};
    private static final int[] noAnomaly1 = {7, 5, 9, 3, 9, 9, 2, 8, 1, 5, 0, 2, 1, 14, 4, 9, 13, 4, 8, 11, 0, 6, 6, 6, 11};
    private static final int[] noAnomaly2 = {1, 13, 2, 8, 3, 9, 10, 11, 9, 2, 8, 7, 0, 0, 2, 4, 1, 13, 0, 6, 4, 11, 13, 0, 3};
    private static final int[] noAnomaly3 = {14, 3, 12, 2, 14, 12, 4, 3, 3, 11, 2, 4, 0, 10, 6, 14, 14, 11, 6, 11, 7, 8, 13, 3, 13};
    private static final int[] noAnomaly4 = {5, 0, 1, 1, 3, 0, 7, 9, 5, 7, 3, 11, 4, 0, 13, 8, 12, 3, 5, 5, 12, 8, 8, 8, 4};
    private static final int[] noAnomaly5 = {9, 13, 5, 10, 2, 9, 7, 7, 13, 12, 10, 7, 14, 1, 11, 4, 8, 8, 1, 3, 2, 11, 7, 13, 14};
    private static final int[] noAnomaly6 = {11, 3, 4, 2, 5, 14, 6, 11, 7, 9, 6, 14, 13, 6, 12, 14, 3, 1, 0, 3, 1, 12, 2, 6, 7};
    private static final int[] noAnomaly7 = {5, 0, 8, 2, 12, 13, 1, 7, 1, 11, 1, 1, 11, 6, 7, 11, 10, 14, 14, 13, 2, 8, 14, 3, 8};
    private static final int[] noAnomaly8 = {1, 7, 5, 5, 14, 14, 11, 3, 9, 0, 2, 10, 4, 4, 13, 0, 1, 8, 5, 6, 11, 0, 8, 8, 5};
    private static final int[] noAnomaly9 = {9, 13, 8, 14, 0, 9, 10, 5, 8, 10, 7, 11, 0, 10, 5, 11, 3, 2, 9, 2, 5, 14, 5, 12, 13};

    private static final int[][] sequences = {
            anomaly340,
            anomaly8505,
            anomaly9044,
            anomaly14774,
            anomaly16860,
            anomaly16877,
            anomaly20596,
            anomaly21867,
            anomaly22361,
            anomaly23154,
            anomaly23953,
            anomaly84391,
            noAnomaly0,
            noAnomaly1,
            noAnomaly2,
            noAnomaly3,
            noAnomaly4,
            noAnomaly5,
            noAnomaly6,
            noAnomaly7,
            noAnomaly8,
            noAnomaly9
    };

    private static final String[] sequenceNames = {
            "anomaly340",
            "anomaly8505",
            "anomaly9044",
            "anomaly14774",
            "anomaly16860",
            "anomaly16877",
            "anomaly20596",
            "anomaly21867",
            "anomaly22361",
            "anomaly23154",
            "anomaly23953",
            "anomaly84391",
            "noAnomaly0",
            "noAnomaly1",
            "noAnomaly2",
            "noAnomaly3",
            "noAnomaly4",
            "noAnomaly5",
            "noAnomaly6",
            "noAnomaly7",
            "noAnomaly8",
            "noAnomaly9"
    };

    /**
     * Maximum possible page reference value.
     */
    private static final int MAX_PAGE_REFERENCE = 16;

    /**
     * Number of simulations to run.
     */
    private static final int MAX_SIMULATIONS = sequences.length;

    /**
     * Maximum number of frames for memory.
     */
    private static final int MAX_FRAME_SIZE = 25;
    
    /**
     * Main method that sets up the simulation, runs it, and reports results.
     */
    public static void main(String[] args) {
        long startTime = System.currentTimeMillis();

        // Executor service to run tasks concurrently
        ExecutorService executor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

        // Arrays to store the page fault counts for FIFO, LRU, and MRU
        int[][] pageFaultsFIFOs = new int[MAX_SIMULATIONS][MAX_FRAME_SIZE + 1];  // Stores page faults for FIFO
        int[][] pageFaultsLRUs = new int[MAX_SIMULATIONS][MAX_FRAME_SIZE + 1];   // Stores page faults for LRU
        int[][] pageFaultsMRUs = new int[MAX_SIMULATIONS][MAX_FRAME_SIZE + 1];   // Stores page faults for MRU

        // Generate and run simulations
        for (int i = 0; i < MAX_SIMULATIONS; i++) {
            int[] sequence = sequences[i];  // Generate a random page reference sequence
            for (int j = 1; j <= MAX_FRAME_SIZE; j++) {
                // Submit tasks to the executor for each frame size and algorithm
                executor.submit(new TaskFIFO(sequence, j, MAX_PAGE_REFERENCE, pageFaultsFIFOs[i]));
                executor.submit(new TaskLRU(sequence, j, MAX_PAGE_REFERENCE, pageFaultsLRUs[i]));
                executor.submit(new TaskMRU(sequence, j, MAX_PAGE_REFERENCE, pageFaultsMRUs[i]));
            }
        }

        // Wait for all tasks to complete
        executor.shutdown();
        try {
            if (!executor.awaitTermination(60, TimeUnit.MINUTES)) {
                executor.shutdownNow();
            }
        } catch (InterruptedException e) {
            executor.shutdownNow();
        }

        // End timing the simulation
        long endTime = System.currentTimeMillis();
        System.out.println("Simulation took: " + (endTime - startTime) + " ms");
        System.out.println();

        // Find the algorithm with the lowest number of page faults
        int fifoMinPF = 0;
        int lruMinPF = 0;
        int mruMinPF = 0;
        for (int i = 0; i < MAX_SIMULATIONS; i++) {
            for (int j = 1; j <= MAX_FRAME_SIZE; j++) {
                int fifoPageFaultCount = pageFaultsFIFOs[i][j];
                int lruPageFaultCount = pageFaultsLRUs[i][j];
                int mruPageFaultCount = pageFaultsMRUs[i][j];

                // Find the minimum number of page faults between the three algorithms
                int min = Math.min(Math.min(fifoPageFaultCount, lruPageFaultCount), mruPageFaultCount);

                // Count how many times each algorithm had the minimum page faults
                if (fifoPageFaultCount == min) {
                    fifoMinPF += 1;
                }
                if (lruPageFaultCount == min) {
                    lruMinPF += 1;
                }
                if (mruPageFaultCount == min) {
                    mruMinPF += 1;
                }
            }
        }
        System.out.println("FIFO min PF: " + fifoMinPF);
        System.out.println("LRU min PF: " + lruMinPF);
        System.out.println("MRU min PF: " + mruMinPF);
        System.out.println();

        // Report on Belady's Anomaly for each algorithm
        reportBeladys("FIFO", pageFaultsFIFOs);
        System.out.println();
        reportBeladys("LRU", pageFaultsLRUs);
        System.out.println();
        reportBeladys("MRU", pageFaultsMRUs);
    }

    /**
     * Reports any instances of Belady's Anomaly for the specified algorithm.
     * Belady's Anomaly occurs when increasing the number of memory frames 
     * results in more page faults.
     *
     * @param title The name of the algorithm (FIFO, LRU, or MRU).
     * @param pageFaults The 2D array of page fault counts for each simulation and frame size.
     */
    private static void reportBeladys(String title, int[][] pageFaults) {
        int anomalyCount = 0;
        int maxDelta = 0;
        System.out.println("Belady's Anomaly Report for " + title);

        // Check for anomalies between frame sizes
        for (int i = 0; i < MAX_SIMULATIONS; i++) {
            for (int j = 1; j < MAX_FRAME_SIZE; j++) {
                for (int k = j; k <= MAX_FRAME_SIZE; k++) {
                    int count1 = pageFaults[i][j];
                    int count2 = pageFaults[i][k];

                    // Detect if page fault count increases as frames increase
                    if (count1 < count2) {
                        anomalyCount += 1;
                        int delta = count2 - count1;
                        maxDelta = Math.max(maxDelta, delta);
                        System.out.println("\tAnomaly detected in simulation #" + i +" (" + sequenceNames[i] + ") - " + count1 + " PF's @ " + j + " frames vs. " + count2 + " PF's @ " + k + " frames (Δ" + delta + ")");
                    }
                }
            }
        }
        System.out.println("Anomaly detected " + anomalyCount + " times in " + MAX_SIMULATIONS + " simulations with a max delta of " + maxDelta);
    }
}
