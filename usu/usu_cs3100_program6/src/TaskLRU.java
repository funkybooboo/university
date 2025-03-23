import java.util.LinkedList;
import java.util.HashSet;
import java.util.Set;

/**
 * The TaskLRU class simulates the Least Recently Used (LRU) page replacement algorithm.
 * It tracks page references and determines the number of page faults that occur when
 * attempting to fit pages into a fixed number of memory frames.
 * <p>
 * The LRU algorithm evicts the least recently used page when memory is full and a new page needs to be loaded.
 */
public class TaskLRU implements Runnable {

    /**
     * The sequence of page references to be processed.
     */
    private final int[] sequence;

    /**
     * The maximum number of memory frames available for storing pages.
     */
    private final int maxMemoryFrames;

    /**
     * The maximum possible page reference value in the sequence.
     */
    private final int maxPageReference;

    /**
     * An array used to store the number of page faults for each simulation with different
     * numbers of memory frames.
     */
    private final int[] pageFaults;

    /**
     * Constructs a TaskLRU object with the given parameters.
     *
     * @param sequence         An array of integers representing the sequence of page references.
     * @param maxMemoryFrames  The maximum number of memory frames available.
     * @param maxPageReference The maximum value of a page reference in the sequence.
     * @param pageFaults       An array to store the number of page faults for each simulation.
     */
    protected TaskLRU(int[] sequence, int maxMemoryFrames, int maxPageReference, int[] pageFaults) {
        this.sequence = sequence;
        this.maxMemoryFrames = maxMemoryFrames;
        this.maxPageReference = maxPageReference;
        this.pageFaults = pageFaults;
    }

    /**
     * Executes the LRU page replacement algorithm.
     * This method simulates processing the sequence of page references and counts the number of page faults
     * that occur when trying to fit the pages into the given number of memory frames.
     */
    @Override
    public void run() {
        // A linked list to simulate memory frames and maintain the order of pages (for LRU)
        LinkedList<Integer> frames = new LinkedList<>();

        // Variable to count the number of page faults during the simulation
        int pageFaultCount = 0;

        // Loop through each page reference in the sequence
        for (int pageReference : sequence) {
            // If the page is not already in memory, it's a page fault
            if (!frames.contains(pageReference)) {
                pageFaultCount++;

                // If memory is full, remove the least recently used page (the first in the list)
                if (frames.size() == maxMemoryFrames) {
                    frames.pollFirst();  // Remove the oldest page
                }

                // Add the new page to memory (most recently used, at the end of the list)
                frames.addLast(pageReference);
            } else {
                // If the page is already in memory, move it to the end (most recently used)
                frames.remove((Integer) pageReference);
                frames.addLast(pageReference);
            }
        }

        // Store the number of page faults for the current number of memory frames
        pageFaults[maxMemoryFrames] = pageFaultCount;
    }
}
