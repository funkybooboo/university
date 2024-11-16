import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class Main {

    private static final int MAX_PAGE_REFERENCE = 250;

    public static void main(String[] args) {
        long startTime = System.currentTimeMillis();
        ExecutorService executor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

        int[][] pageFaultsFIFOs = new int[1000][101];  // Stores page faults for FIFO for each simulation and frame size
        int[][] pageFaultsLRUs = new int[1000][101];   // Stores page faults for LRU
        int[][] pageFaultsMRUs = new int[1000][101];   // Stores page faults for MRU

        for (int simulation_id = 0; simulation_id < 1000; simulation_id++) { // Start from 0
            int[] sequence = getRandomSequence(1000);

            for (int maxMemoryFrames = 1; maxMemoryFrames <= 100; maxMemoryFrames++) {
                Runnable taskFIFO = new TaskFIFO(sequence, maxMemoryFrames, MAX_PAGE_REFERENCE, pageFaultsFIFOs[simulation_id]);
                Runnable taskLRU = new TaskLRU(sequence, maxMemoryFrames, MAX_PAGE_REFERENCE, pageFaultsLRUs[simulation_id]);
                Runnable taskMRU = new TaskMRU(sequence, maxMemoryFrames, MAX_PAGE_REFERENCE, pageFaultsMRUs[simulation_id]);

                // Submit tasks to the executor
                executor.submit(taskFIFO);
                executor.submit(taskLRU);
                executor.submit(taskMRU);
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

        // Now compute results for each frame size (1-100)
        int fifoMinPF = 0;
        int lruMinPF = 0;
        int mruMinPF = 0;

        int anomalyCountFIFO = 0;  // FIFO anomaly count
        int anomalyCountLRU = 0;   // LRU anomaly count
        int anomalyCountMRU = 0;   // MRU anomaly count

        int maxDeltaFIFO = 0;  // Maximum delta for FIFO anomalies
        int maxDeltaLRU = 0;   // Maximum delta for LRU anomalies
        int maxDeltaMRU = 0;   // Maximum delta for MRU anomalies

        List<String> anomaliesFIFO = new ArrayList<>();
        List<String> anomaliesLRU = new ArrayList<>();
        List<String> anomaliesMRU = new ArrayList<>();

        // Iterate through frame sizes (1 to 100)
        for (int maxMemoryFrames = 1; maxMemoryFrames <= 100; maxMemoryFrames++) {
            // Track which algorithm had the minimum page faults for this frame size across all simulations
            int minPageFaults = Integer.MAX_VALUE;

            for (int simulation_id = 0; simulation_id < 1000; simulation_id++) {
                int fifoPageFaults = pageFaultsFIFOs[simulation_id][maxMemoryFrames];
                int lruPageFaults = pageFaultsLRUs[simulation_id][maxMemoryFrames];
                int mruPageFaults = pageFaultsMRUs[simulation_id][maxMemoryFrames];

                // Track min page faults across algorithms
                if (fifoPageFaults < minPageFaults) {
                    minPageFaults = fifoPageFaults;
                    fifoMinPF++;
                }
                if (lruPageFaults < minPageFaults) {
                    minPageFaults = lruPageFaults;
                    lruMinPF++;
                }
                if (mruPageFaults < minPageFaults) {
                    minPageFaults = mruPageFaults;
                    mruMinPF++;
                }
            }

            // Check for Belady's Anomaly between consecutive frame sizes
            if (maxMemoryFrames > 1) {
                for (int simulation_id = 0; simulation_id < 1000; simulation_id++) {
                    // FIFO Anomaly Check
                    if (pageFaultsFIFOs[simulation_id][maxMemoryFrames] > pageFaultsFIFOs[simulation_id][maxMemoryFrames - 1]) {
                        anomalyCountFIFO++;
                        int delta = pageFaultsFIFOs[simulation_id][maxMemoryFrames] - pageFaultsFIFOs[simulation_id][maxMemoryFrames - 1];
                        maxDeltaFIFO = Math.max(maxDeltaFIFO, delta);
                        anomaliesFIFO.add(String.format("Anomaly detected in simulation #%03d - %d PF's @ %d frames vs. %d PF's @ %d frames (Δ%d)",
                                simulation_id, pageFaultsFIFOs[simulation_id][maxMemoryFrames], maxMemoryFrames,
                                pageFaultsFIFOs[simulation_id][maxMemoryFrames - 1], maxMemoryFrames - 1, delta));
                    }

                    // LRU Anomaly Check
                    if (pageFaultsLRUs[simulation_id][maxMemoryFrames] > pageFaultsLRUs[simulation_id][maxMemoryFrames - 1]) {
                        anomalyCountLRU++;
                        int delta = pageFaultsLRUs[simulation_id][maxMemoryFrames] - pageFaultsLRUs[simulation_id][maxMemoryFrames - 1];
                        maxDeltaLRU = Math.max(maxDeltaLRU, delta);
                        anomaliesLRU.add(String.format("Anomaly detected in simulation #%03d - %d PF's @ %d frames vs. %d PF's @ %d frames (Δ%d)",
                                simulation_id, pageFaultsLRUs[simulation_id][maxMemoryFrames], maxMemoryFrames,
                                pageFaultsLRUs[simulation_id][maxMemoryFrames - 1], maxMemoryFrames - 1, delta));
                    }

                    // MRU Anomaly Check
                    if (pageFaultsMRUs[simulation_id][maxMemoryFrames] > pageFaultsMRUs[simulation_id][maxMemoryFrames - 1]) {
                        anomalyCountMRU++;
                        int delta = pageFaultsMRUs[simulation_id][maxMemoryFrames] - pageFaultsMRUs[simulation_id][maxMemoryFrames - 1];
                        maxDeltaMRU = Math.max(maxDeltaMRU, delta);
                        anomaliesMRU.add(String.format("Anomaly detected in simulation #%03d - %d PF's @ %d frames vs. %d PF's @ %d frames (Δ%d)",
                                simulation_id, pageFaultsMRUs[simulation_id][maxMemoryFrames], maxMemoryFrames,
                                pageFaultsMRUs[simulation_id][maxMemoryFrames - 1], maxMemoryFrames - 1, delta));
                    }
                }
            }
        }

        // Print results
        System.out.printf("FIFO min PF: %d\n", fifoMinPF);
        System.out.printf("LRU min PF: %d\n", lruMinPF);
        System.out.printf("MRU min PF: %d\n", mruMinPF);

        // Print Belady's Anomaly report for FIFO, LRU, and MRU
        System.out.println("\nBelady's Anomaly Report for FIFO");
        anomaliesFIFO.forEach(System.out::println);
        System.out.println("Anomaly detected " + anomalyCountFIFO + " times with a max delta of " + maxDeltaFIFO);
        
        System.out.println("\nBelady's Anomaly Report for LRU");
        anomaliesLRU.forEach(System.out::println);
        System.out.println("Anomaly detected " + anomalyCountLRU + " times with a max delta of " + maxDeltaLRU);

        System.out.println("\nBelady's Anomaly Report for MRU");
        anomaliesMRU.forEach(System.out::println);
        System.out.println("Anomaly detected " + anomalyCountMRU + " times with a max delta of " + maxDeltaMRU);
    }

    // Generate a random page reference sequence of given length
    public static int[] getRandomSequence(int length) {
        Random rand = new Random();
        int[] sequence = new int[length];
        for (int i = 0; i < length; i++) {
            sequence[i] = rand.nextInt(MAX_PAGE_REFERENCE) + 1; // Random page reference between 1 and 250
        }
        return sequence;
    }
}
