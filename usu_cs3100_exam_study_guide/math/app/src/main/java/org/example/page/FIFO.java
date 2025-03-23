package org.example.page;

import java.util.LinkedList;
import java.util.Queue;

public class FIFO {

    private final int[] sequence;
    private final int maxMemoryFrames;
    private final int[] pageFaults;

    protected FIFO(int[] sequence, int maxMemoryFrames, int[] pageFaults) {
        this.sequence = sequence;
        this.maxMemoryFrames = maxMemoryFrames;
        this.pageFaults = pageFaults;
    }

    public void run() {
        // A queue to simulate memory frames, storing pages in a FIFO order
        Queue<Integer> frames = new LinkedList<>();

        // Variable to track the number of page faults during the simulation
        int pageFaultCount = 0;

        // Process each page reference in the sequence
        for (int pageReference : sequence) {
            System.out.println("Processing page: " + pageReference);

            // If the page is not in memory, it's a page fault
            if (!frames.contains(pageReference)) {
                pageFaultCount++;
                System.out.println("Page fault! Adding page " + pageReference + " to memory.");

                // If memory is full, remove the oldest page (the first page in the queue)
                if (frames.size() == maxMemoryFrames) {
                    int removedPage = frames.poll();
                    System.out.println("Memory full. Swapping out page " + removedPage);
                }

                // Add the new page to the memory
                frames.offer(pageReference);
            } else {
                System.out.println("Page " + pageReference + " is already in memory.");
            }

            // Print the current state of the memory frames
            System.out.println("Current memory frames: " + frames);
            System.out.println("-----------------------------");
        }

        // Store the number of page faults for the current number of memory frames
        pageFaults[maxMemoryFrames] = pageFaultCount;

        // At the end, print the total number of page faults
        System.out.println("Total page faults: " + pageFaultCount);
    }
}
