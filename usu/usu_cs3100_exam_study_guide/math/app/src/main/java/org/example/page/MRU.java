package org.example.page;

import java.util.LinkedList;

public class MRU {

    private final int[] sequence;
    private final int maxMemoryFrames;
    private final int[] pageFaults;

    protected MRU(int[] sequence, int maxMemoryFrames, int[] pageFaults) {
        this.sequence = sequence;
        this.maxMemoryFrames = maxMemoryFrames;
        this.pageFaults = pageFaults;
    }

    public void run() {
        // LinkedList to simulate frames as a queue (used for memory)
        LinkedList<Integer> frames = new LinkedList<>();

        // Variable to count the number of page faults
        int pageFaultCount = 0;

        // Loop through each page reference in the sequence
        for (int page : sequence) {
            System.out.println("Processing page: " + page);

            // If the page is not in frames, it's a page fault
            if (!frames.contains(page)) {
                pageFaultCount++;
                System.out.println("Page fault! Adding page " + page + " to memory.");

                // If frames is full, remove the most recently used page (the last element in the list)
                if (frames.size() == maxMemoryFrames) {
                    int removedPage = frames.removeLast();  // Remove the most recently used page
                    System.out.println("Memory full. Swapping out page " + removedPage);
                }

                // Add the new page to frames (this is now the most recently used)
                frames.add(page);
            } else {
                // If the page is already in frames, remove it and add it back as the most recently used
                frames.remove((Integer) page);  // Remove the page
                frames.add(page);               // Add it back as most recently used
                System.out.println("Page " + page + " is already in memory. Moving to the most recent.");
            }

            // Print the current state of the memory frames
            System.out.println("Current memory frames: " + frames);
            System.out.println("-----------------------------");
        }

        // Store the number of page faults for the current simulation with maxMemoryFrames
        pageFaults[maxMemoryFrames] = pageFaultCount;

        // Print the total number of page faults at the end of the run
        System.out.println("Total page faults: " + pageFaultCount);
    }
}
