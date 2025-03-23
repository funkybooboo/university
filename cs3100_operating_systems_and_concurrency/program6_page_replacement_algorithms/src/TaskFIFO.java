import java.util.LinkedList;
import java.util.Queue;

/**
 * The TaskFIFO class simulates the First-In-First-Out (FIFO) page replacement algorithm.
 * It manages the sequence of page references and tracks the number of page faults that occur
 * when trying to fit the pages into a fixed number of memory frames.
 * <p>
 * The FIFO algorithm removes the oldest page in memory when a new page needs to be loaded,
 * provided there is no space left in memory.
 */
public class TaskFIFO implements Runnable {

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
     * An array used to record the number of page faults for each simulation with
     * different numbers of memory frames.
     */
    private final int[] pageFaults;

    /**
     * Constructs a TaskFIFO object with the given parameters.
     *
     * @param sequence         An array of integers representing the sequence of page references.
     * @param maxMemoryFrames  The maximum number of memory frames available.
     * @param maxPageReference The maximum value of a page reference in the sequence.
     * @param pageFaults       An array to store the number of page faults for each simulation.
     */
    protected TaskFIFO(int[] sequence, int maxMemoryFrames, int maxPageReference, int[] pageFaults) {
        this.sequence = sequence;
        this.maxMemoryFrames = maxMemoryFrames;
        this.maxPageReference = maxPageReference;
        this.pageFaults = pageFaults;
    }

    /**
     * Executes the FIFO page replacement algorithm.
     * This method simulates processing the sequence of page references and counts the number of page faults
     * that occur for the given number of memory frames.
     */
    @Override
    public void run() {
        // A queue to simulate memory frames, storing pages in a FIFO order
        Queue<Integer> frames = new LinkedList<>();

        // Variable to track the number of page faults during the simulation
        int pageFaultCount = 0;

        // Process each page reference in the sequence
        for (int pageReference : sequence) {
            // If the page is not in memory, it's a page fault
            if (!frames.contains(pageReference)) {
                pageFaultCount++;

                // If memory is full, remove the oldest page (the first page in the queue)
                if (frames.size() == maxMemoryFrames) {
                    frames.poll();
                }

                // Add the new page to the memory
                frames.offer(pageReference);
            }
        }

        // Store the number of page faults for the current number of memory frames
        pageFaults[maxMemoryFrames] = pageFaultCount;
    }
}
