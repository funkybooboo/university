package org.example.page;

public class OPT {
    private final int[] sequence;
    private final int maxMemoryFrames;
    private final int[] pageFaults;

    protected OPT(int[] sequence, int maxMemoryFrames, int[] pageFaults) {
        this.sequence = sequence;
        this.maxMemoryFrames = maxMemoryFrames;
        this.pageFaults = pageFaults;
    }

    public void run() {
        // Array to simulate memory frames (pages currently in memory)
        int[] frames = new int[maxMemoryFrames];
        for (int i = 0; i < maxMemoryFrames; i++) {
            frames[i] = -1;  // -1 means that the frame is empty
        }

        // Variable to count page faults
        int pageFaultCount = 0;

        // Iterate over each page reference in the sequence
        for (int i = 0; i < sequence.length; i++) {
            int pageReference = sequence[i];
            System.out.println("Processing page: " + pageReference);

            // Check if the page is already in memory (no page fault)
            boolean pageHit = false;
            for (int j = 0; j < maxMemoryFrames; j++) {
                if (frames[j] == pageReference) {
                    pageHit = true; // Page is in memory
                    break;
                }
            }

            // If the page is not in memory, it's a page fault
            if (!pageHit) {
                pageFaultCount++;
                System.out.println("Page fault! Adding page " + pageReference + " to memory.");

                // Check if there is space in memory for the new page
                boolean placed = false;
                for (int j = 0; j < maxMemoryFrames; j++) {
                    if (frames[j] == -1) {  // If the frame is empty
                        frames[j] = pageReference;
                        placed = true;
                        break;
                    }
                }

                // If the page was not placed (memory is full), evict a page
                if (!placed) {
                    // Find the page that will be used furthest in the future
                    int farthestIndex = -1;
                    int pageToEvict = -1;

                    for (int j = 0; j < maxMemoryFrames; j++) {
                        int nextUse = -1;
                        // Look ahead to see when the current frame will be accessed again
                        for (int k = i + 1; k < sequence.length; k++) {
                            if (frames[j] == sequence[k]) {
                                nextUse = k;
                                break;
                            }
                        }

                        // If the page is not accessed in the future, evict it
                        if (nextUse == -1) {
                            pageToEvict = frames[j];
                            frames[j] = pageReference;
                            break;
                        }

                        // Track the page that is used the furthest in the future
                        if (nextUse > farthestIndex) {
                            farthestIndex = nextUse;
                            pageToEvict = frames[j];
                        }
                    }

                    // After identifying the page to evict, update the frames
                    // Replace the pageToEvict with the new pageReference
                    System.out.println("Memory full. Swapping out page " + pageToEvict);
                    for (int j = 0; j < maxMemoryFrames; j++) {
                        if (frames[j] == pageToEvict) {
                            frames[j] = pageReference;
                            break;
                        }
                    }
                }
            }

            // Print the current state of memory frames
            System.out.print("Current memory frames: [");
            for (int j = 0; j < maxMemoryFrames; j++) {
                System.out.print(frames[j] == -1 ? "empty" : frames[j]);
                if (j < maxMemoryFrames - 1) System.out.print(", ");
            }
            System.out.println("]");
            System.out.println("-----------------------------");
        }

        // Store the number of page faults for the simulation with maxMemoryFrames
        pageFaults[maxMemoryFrames] = pageFaultCount;

        // Print the total number of page faults at the end of the run
        System.out.println("Total page faults: " + pageFaultCount);
    }
}
