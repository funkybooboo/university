import java.util.Random;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

/**
 * The Assign6 class simulates page replacement algorithms (FIFO, LRU, and MRU)
 * and evaluates their performance through a series of simulations.
 * It tracks page faults for each algorithm and performs a comparison to detect 
 * Belady's Anomaly, where the page fault count increases with more frames.
 */
public class Assign6 {

    /**
     * Maximum possible page reference value.
     */
    private static final int MAX_PAGE_REFERENCE = 250;

    /**
     * Number of simulations to run.
     */
    private static final int MAX_SIMULATIONS = 1000;

    /**
     * Maximum number of frames for memory.
     */
    private static final int MAX_FRAME_SIZE = 100;

    /**
     * Length of the page reference string.
     */
    private static final int PAGE_REFERENCE_STRING_SIZE = 1000;

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
            int[] sequence = getRandomSequence(PAGE_REFERENCE_STRING_SIZE);  // Generate a random page reference sequence
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
                        System.out.println("\tAnomaly detected in simulation #" + i + " - " + count1 + " PF's @ " + j + " frames vs. " + count2 + " PF's @ " + k + " frames (Δ" + delta + ")");
                    }
                }
            }
        }
        System.out.println("Anomaly detected " + anomalyCount + " times in " + MAX_SIMULATIONS + " simulations with a max delta of " + maxDelta);
    }

    /**
     * Generates a random page reference sequence.
     *
     * @param length The length of the page reference sequence.
     * @return An array containing the randomly generated page reference sequence.
     */
    public static int[] getRandomSequence(int length) {
        Random rand = new Random();
        int[] sequence = new int[length];
        for (int i = 0; i < length; i++) {
            sequence[i] = rand.nextInt(MAX_PAGE_REFERENCE) + 1; // Random page reference between 1 and 250
        }
        return sequence;
    }
}
