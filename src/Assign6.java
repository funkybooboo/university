import java.util.Random;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class Assign6 {

    private static final int MAX_PAGE_REFERENCE = 250;
    private static final int MAX_SIMULATIONS = 1000;
    private static final int MAX_FRAME_SIZE = 100;
    private static final int PAGE_REFERENCE_STRING_SIZE = 1000;

    public static void main(String[] args) {
        long startTime = System.currentTimeMillis();
        ExecutorService executor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

        int[][] pageFaultsFIFOs = new int[MAX_SIMULATIONS][MAX_FRAME_SIZE + 1];  // Stores page faults for FIFO for each simulation and frame size
        int[][] pageFaultsLRUs = new int[MAX_SIMULATIONS][MAX_FRAME_SIZE + 1];   // Stores page faults for LRU
        int[][] pageFaultsMRUs = new int[MAX_SIMULATIONS][MAX_FRAME_SIZE + 1];   // Stores page faults for MRU

        for (int i = 0; i < MAX_SIMULATIONS; i++) {
            int[] sequence = getRandomSequence(PAGE_REFERENCE_STRING_SIZE);
            for (int j = 1; j <= MAX_FRAME_SIZE; j++) {
                // Submit tasks to the executor
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

        // Lowest number of page faults
        int fifoMinPF = 0;
        int lruMinPF = 0;
        int mruMinPF = 0;
        for (int i = 0; i < MAX_SIMULATIONS; i++) {
            for (int j = 1; j <= MAX_FRAME_SIZE; j++) {
                int fifoPageFaultCount = pageFaultsFIFOs[i][j];
                int lruPageFaultCount = pageFaultsLRUs[i][j];
                int mruPageFaultCount = pageFaultsMRUs[i][j];
                
                int min = Math.min(Math.min(fifoPageFaultCount, lruPageFaultCount), mruPageFaultCount);
                
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
        System.out.println("FIFO min PF: "+fifoMinPF);
        System.out.println("LRU min PF: "+lruMinPF);
        System.out.println("MRU min PF: "+mruMinPF);
        System.out.println();

        reportBeladys("FIFO", pageFaultsFIFOs);
        System.out.println();
        reportBeladys("LRU", pageFaultsLRUs);
        System.out.println();
        reportBeladys("MRU", pageFaultsMRUs);
    }

    private static void reportBeladys(String title, int[][] pageFaultsFIFOs) {
        int anomalyCount = 0;
        int maxDelta = 0;
        System.out.println("Belady's Anomaly Report for "+title);
        for (int i = 0; i < MAX_SIMULATIONS; i++) {
            for (int j = 1; j < MAX_FRAME_SIZE; j++) {
                for (int k = j; k <= MAX_FRAME_SIZE; k++) {
                    int count1 = pageFaultsFIFOs[i][j];
                    int count2 = pageFaultsFIFOs[i][k];
                    if (count1 < count2) {
                        anomalyCount += 1;
                        int delta = count2 - count1;
                        maxDelta = Math.max(maxDelta, delta);
                        System.out.println("\tAnomaly detected in simulation #"+i+" - "+count1+" PF's @  "+j+" frames vs. "+count2+" PF's @  "+k+" frames (Δ"+delta+")");
                    }
                }
            }
        }
        System.out.println("Anomaly detected "+anomalyCount+" times in "+MAX_SIMULATIONS+" simulations with a max delta of "+maxDelta);
    }

    public static int[] getRandomSequence(int length) {
        Random rand = new Random();
        int[] sequence = new int[length];
        for (int i = 0; i < length; i++) {
            sequence[i] = rand.nextInt(MAX_PAGE_REFERENCE) + 1; // Random page reference between 1 and 250
        }
        return sequence;
    }
}
