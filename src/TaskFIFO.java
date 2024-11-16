import java.util.LinkedList;
import java.util.Queue;

public class TaskFIFO implements Runnable {

    private final int[] sequence;
    private final int maxMemoryFrames;
    private final int maxPageReference;
    private final int[] pageFaults; // output

    /*
     * sequence : (input) a randomly generated sequence of page references
     * maxMemoryFrames : (input) the number of frames of memory available
     * maxPageReference : (input) the maximum page reference possible in the sequence
     * pageFaults : (output) an array used to record the number of page faults that occur each simulation of some number of frames.  Each call to the 'run' method of a task results in storing the number of page faults for the task using something like: pageFaults[maxMemoryFrames] = pageFaults (where pageFaults is the number of page faults your code detects).
     */
    protected TaskFIFO(int[] sequence, int maxMemoryFrames, int maxPageReference, int[] pageFaults) {
        this.sequence = sequence;
        this.maxMemoryFrames = maxMemoryFrames;
        this.maxPageReference = maxPageReference;
        this.pageFaults = pageFaults;
    }

    @Override
    public void run() {
        Queue<Integer> memory = new LinkedList<>();

        int pageFaultCount = 0;

        for (int page : sequence) {
            if (!memory.contains(page)) {
                pageFaultCount++;

                if (memory.size() == maxMemoryFrames) {
                    memory.poll();
                }

                memory.offer(page);
            }
        }
        
        pageFaults[maxMemoryFrames] = pageFaultCount;
    }
}
