import java.util.LinkedList;

/**
 * The TaskMRU class simulates the Most Recently Used (MRU) page replacement algorithm.
 * It tracks page references and determines the number of page faults that occur
 * when attempting to fit pages into a fixed number of memory frames.
 * <p>
 * The MRU algorithm evicts the most recently used page when memory is full
 * and a new page needs to be loaded.
 */
public class TaskMRU implements Runnable {

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
    private final int[] pageFaults; // output

    /**
     * Constructs a TaskMRU object with the given parameters.
     *
     * @param sequence         An array of integers representing the sequence of page references.
     * @param maxMemoryFrames  The maximum number of memory frames available.
     * @param maxPageReference The maximum value of a page reference in the sequence.
     * @param pageFaults       An array to store the number of page faults for each simulation.
     */
    protected TaskMRU(int[] sequence, int maxMemoryFrames, int maxPageReference, int[] pageFaults) {
        this.sequence = sequence;
        this.maxMemoryFrames = maxMemoryFrames;
        this.maxPageReference = maxPageReference;
        this.pageFaults = pageFaults;
    }

    /**
     * Executes the MRU page replacement algorithm.
     * This method simulates processing the sequence of page references and counts the number of page faults
     * that occur when trying to fit the pages into the given number of memory frames.
     * <p>
     * The MRU algorithm evicts the most recently used page when a new page needs to be loaded,
     * rather than the least recently used page as in other algorithms.
     */
    @Override
    public void run() {
        // LinkedList to simulate frames as a queue
        LinkedList<Integer> frames = new LinkedList<>();

        // Variable to count the number of page faults
        int pageFaultCount = 0;

        // Loop through each page reference in the sequence
        for (int page : sequence) {
            // If the page is not in frames, it's a page fault
            if (!frames.contains(page)) {
                pageFaultCount++;

                // If frames is full, remove the most recently used page (last element in the list)
                if (frames.size() == maxMemoryFrames) {
                    frames.removeLast();
                }

                // Add the new page to frames
                frames.add(page);
            } else {
                // If the page is already in frames, remove it and add it back to the end (most recently used)
                frames.remove((Integer) page);
                frames.add(page);
            }
        }

        // Store the number of page faults for the current simulation with maxMemoryFrames
        pageFaults[maxMemoryFrames] = pageFaultCount;
    }
}
