public class Test {

    public static void main(String[] args) {
        
        System.out.println("LRU");
        testLRU();
        
        System.out.println("MRU");
        testMRU();

        System.out.println("test");
        testSequences();
    }
    
    private static final int MAX_PAGE_REFERENCE = 250;

    private static void testLRU() {
        int[] sequence1 = {1, 2, 3, 4, 5, 6, 7, 8, 9};
        int[] sequence2 = {1, 2, 1, 3, 2, 1, 2, 3, 4};
        int[] pageFaults = new int[4];  // 4 because maxMemoryFrames is 3

        // Replacement should be: 1, 2, 3, 4, 5, 6, 7, 8
        // Page Faults should be 9
        (new TaskLRU(sequence1, 1, MAX_PAGE_REFERENCE, pageFaults)).run();
        System.out.printf("Page Faults: %d\n", pageFaults[1]);

        // Replacement should be: 2, 1, 3, 1, 2
        // Page Faults should be 7
        (new TaskLRU(sequence2, 2, MAX_PAGE_REFERENCE, pageFaults)).run();
        System.out.printf("Page Faults: %d\n", pageFaults[2]);

        // Replacement should be: 1
        // Page Faults should be 4
        (new TaskLRU(sequence2, 3, MAX_PAGE_REFERENCE, pageFaults)).run();
        System.out.printf("Page Faults: %d\n", pageFaults[3]);
    }

    private static void testMRU() {
        int[] sequence1 = {1, 2, 3, 4, 5, 6, 7, 8, 9};
        int[] sequence2 = {1, 2, 1, 3, 2, 1, 2, 3, 4};
        int[] pageFaults = new int[4];  // 4 because maxMemoryFrames is 3

        // Replacement should be: 1, 2, 3, 4, 5, 6, 7, 8
        // Page Faults should be 9
        (new TaskMRU(sequence1, 1, MAX_PAGE_REFERENCE, pageFaults)).run();
        System.out.printf("Page Faults: %d\n", pageFaults[1]);

        // Replacement should be: 1, 2, 1, 3
        // Page Faults should be 6
        (new TaskMRU(sequence2, 2, MAX_PAGE_REFERENCE, pageFaults)).run();
        System.out.printf("Page Faults: %d\n", pageFaults[2]);

        // Replacement should be: 3
        // Page Faults should be 4
        (new TaskMRU(sequence2, 3, MAX_PAGE_REFERENCE, pageFaults)).run();
        System.out.printf("Page Faults: %d\n", pageFaults[3]);
    }

    private static void testSequences() {
        // Anomaly detected in simulation #340 - 16 PF's @   6 frames vs. 17 PF's @   7 frames (Δ1)
        int[] anomaly340 = {11, 2, 5, 1, 11, 2, 10, 3, 5, 0, 13, 2, 10, 5, 9, 11, 7, 7, 2, 14, 5, 7, 12, 10, 9};
        // Anomaly detected in simulation #8505 - 17 PF's @   6 frames vs. 18 PF's @   7 frames (Δ1)
        int[] anomaly8505 = {13, 3, 7, 11, 1, 7, 3, 6, 9, 4, 3, 12, 1, 7, 14, 0, 11, 3, 7, 0, 8, 1, 11, 12, 14};
        // Anomaly detected in simulation #9044 - 15 PF's @   5 frames vs. 16 PF's @   6 frames (Δ1)
        int[] anomaly9044 = {1, 9, 4, 5, 12, 8, 1, 10, 4, 8, 9, 14, 2, 9, 3, 14, 4, 5, 14, 6, 14, 5, 5, 2, 2};
        // Anomaly detected in simulation #14774 - 17 PF's @   4 frames vs. 18 PF's @   5 frames (Δ1)
        int[] anomaly14774 = {1, 6, 9, 11, 2, 2, 4, 6, 0, 9, 12, 6, 9, 0, 14, 1, 8, 1, 12, 3, 8, 6, 8, 11, 14};
        // Anomaly detected in simulation #16860 - 16 PF's @   4 frames vs. 17 PF's @   5 frames (Δ1)
        int[] anomaly16860 = {13, 0, 4, 5, 13, 5, 7, 13, 1, 4, 2, 13, 4, 9, 11, 5, 9, 2, 1, 11, 13, 1, 11, 10, 4};
        // Anomaly detected in simulation #16877 - 15 PF's @   5 frames vs. 16 PF's @   6 frames (Δ1)
        int[] anomaly16877 = {5, 3, 14, 8, 8, 11, 1, 10, 3, 14, 14, 1, 7, 0, 0, 13, 14, 3, 4, 13, 12, 7, 3, 0, 0};
        // Anomaly detected in simulation #20596 - 15 PF's @   5 frames vs. 16 PF's @   6 frames (Δ1)
        int[] anomaly20596 = {11, 10, 3, 14, 12, 12, 3, 7, 9, 10, 3, 10, 6, 2, 3, 10, 10, 11, 6, 13, 1, 2, 2, 6, 4};
        // Anomaly detected in simulation #21867 - 16 PF's @   5 frames vs. 17 PF's @   6 frames (Δ1)
        int[] anomaly21867 = {3, 11, 14, 10, 1, 1, 5, 4, 14, 7, 14, 4, 11, 10, 0, 14, 1, 8, 3, 1, 1, 10, 3, 11, 0};
        // Anomaly detected in simulation #22361 - 18 PF's @   6 frames vs. 19 PF's @   7 frames (Δ1)
        int[] anomaly22361 = {9, 8, 2, 1, 10, 0, 5, 9, 12, 8, 14, 3, 9, 8, 5, 6, 11, 2, 6, 5, 4, 6, 3, 9, 2};
        // Anomaly detected in simulation #23154 - 17 PF's @   6 frames vs. 18 PF's @   7 frames (Δ1)
        int[] anomaly23154 = {13, 0, 4, 7, 4, 9, 13, 9, 3, 10, 8, 8, 0, 4, 2, 0, 4, 6, 5, 9, 14, 5, 7, 2, 13};
        // Anomaly detected in simulation #23953 - 17 PF's @   5 frames vs. 18 PF's @   6 frames (Δ1)
        int[] anomaly23953 = {9, 5, 10, 6, 8, 1, 3, 6, 1, 3, 5, 11, 8, 10, 2, 5, 6, 12, 1, 12, 10, 7, 2, 5, 11};
        // Anomaly detected in simulation #84391 - 18 PF's @   4 frames vs. 19 PF's @   5 frames (Δ1)
        int[] anomaly84391 = {13, 14, 6, 0, 3, 2, 9, 0, 6, 11, 0, 10, 10, 0, 6, 3, 13, 7, 10, 11, 8, 8, 4, 1, 1};

        // There are no anomalies in these sequences
        int[] noAnomaly0 = {1, 7, 14, 11, 1, 13, 11, 12, 0, 5, 9, 3, 2, 0, 2, 12, 10, 2, 11, 6, 2, 7, 14, 6, 14};
        int[] noAnomaly1 = {7, 5, 9, 3, 9, 9, 2, 8, 1, 5, 0, 2, 1, 14, 4, 9, 13, 4, 8, 11, 0, 6, 6, 6, 11};
        int[] noAnomaly2 = {1, 13, 2, 8, 3, 9, 10, 11, 9, 2, 8, 7, 0, 0, 2, 4, 1, 13, 0, 6, 4, 11, 13, 0, 3};
        int[] noAnomaly3 = {14, 3, 12, 2, 14, 12, 4, 3, 3, 11, 2, 4, 0, 10, 6, 14, 14, 11, 6, 11, 7, 8, 13, 3, 13};
        int[] noAnomaly4 = {5, 0, 1, 1, 3, 0, 7, 9, 5, 7, 3, 11, 4, 0, 13, 8, 12, 3, 5, 5, 12, 8, 8, 8, 4};
        int[] noAnomaly5 = {9, 13, 5, 10, 2, 9, 7, 7, 13, 12, 10, 7, 14, 1, 11, 4, 8, 8, 1, 3, 2, 11, 7, 13, 14};
        int[] noAnomaly6 = {11, 3, 4, 2, 5, 14, 6, 11, 7, 9, 6, 14, 13, 6, 12, 14, 3, 1, 0, 3, 1, 12, 2, 6, 7};
        int[] noAnomaly7 = {5, 0, 8, 2, 12, 13, 1, 7, 1, 11, 1, 1, 11, 6, 7, 11, 10, 14, 14, 13, 2, 8, 14, 3, 8};
        int[] noAnomaly8 = {1, 7, 5, 5, 14, 14, 11, 3, 9, 0, 2, 10, 4, 4, 13, 0, 1, 8, 5, 6, 11, 0, 8, 8, 5};
        int[] noAnomaly9 = {9, 13, 8, 14, 0, 9, 10, 5, 8, 10, 7, 11, 0, 10, 5, 11, 3, 2, 9, 2, 5, 14, 5, 12, 13};
    }

    private static void test(int[] sequence) {
        for (int maxMemoryFrames = 1; maxMemoryFrames <= 100; maxMemoryFrames++) {
            int[] pageFaults = new int[101]; 
            (new TaskFIFO(sequence, maxMemoryFrames, MAX_PAGE_REFERENCE, pageFaults)).run();
            System.out.printf("Page Faults: %d\n", pageFaults[1]);
        }

        for (int maxMemoryFrames = 1; maxMemoryFrames <= 100; maxMemoryFrames++) {
            int[] pageFaults = new int[101];
            (new TaskMRU(sequence, maxMemoryFrames, MAX_PAGE_REFERENCE, pageFaults)).run();
            System.out.printf("Page Faults: %d\n", pageFaults[1]);
        }


        for (int maxMemoryFrames = 1; maxMemoryFrames <= 100; maxMemoryFrames++) {
            int[] pageFaults = new int[101];
            (new TaskLRU(sequence, maxMemoryFrames, MAX_PAGE_REFERENCE, pageFaults)).run();
            System.out.printf("Page Faults: %d\n", pageFaults[1]);
        }
    }
}
