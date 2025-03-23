package org.example.page;

import java.util.LinkedList;

public class LRU {

    private final int[] sequence;
    private final int maxMemoryFrames;
    private final int[] pageFaults;

    protected LRU(int[] sequence, int maxMemoryFrames, int[] pageFaults) {
        this.sequence = sequence;
        this.maxMemoryFrames = maxMemoryFrames;
        this.pageFaults = pageFaults;
    }

    public void run() {
        // A linked list to simulate memory frames and maintain the order of pages (for LRU)
        LinkedList<Integer> frames = new LinkedList<>();

        // Variable to count the number of page faults during the simulation
        int pageFaultCount = 0;

        // Loop through each page reference in the sequence
        for (int pageReference : sequence) {
            System.out.println("Processing page: " + pageReference);

            // If the page is not already in memory, it's a page fault
            if (!frames.contains(pageReference)) {
                pageFaultCount++;
                System.out.println("Page fault! Adding page " + pageReference + " to memory.");

                // If memory is full, remove the least recently used page (the first in the list)
                if (frames.size() == maxMemoryFrames) {
                    int removedPage = frames.pollFirst();  // Remove the least recently used page
                    System.out.println("Memory full. Swapping out page " + removedPage);
                }

                // Add the new page to memory (most recently used, at the end of the list)
                frames.addLast(pageReference);
            } else {
                // If the page is already in memory, move it to the end (most recently used)
                frames.remove((Integer) pageReference);  // Remove the page
                frames.addLast(pageReference);           // Add it back as most recently used
                System.out.println("Page " + pageReference + " is already in memory. Moving to the most recent.");
            }

            // Print the current state of the memory frames
            System.out.println("Current memory frames: " + frames);
            System.out.println("-----------------------------");
        }

        // Store the number of page faults for the current number of memory frames
        pageFaults[maxMemoryFrames] = pageFaultCount;

        // Print the total number of page faults at the end of the run
        System.out.println("Total page faults: " + pageFaultCount);
    }
}
