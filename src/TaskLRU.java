import java.util.LinkedList;
import java.util.HashSet;
import java.util.Set;

public class TaskLRU implements Runnable {
    private final int[] sequence;
    private final int maxMemoryFrames;
    private final int maxPageReference;
    private final int[] pageFaults; 

    protected TaskLRU(int[] sequence, int maxMemoryFrames, int maxPageReference, int[] pageFaults) {
        this.sequence = sequence;
        this.maxMemoryFrames = maxMemoryFrames;
        this.maxPageReference = maxPageReference;
        this.pageFaults = pageFaults;
    }

    @Override
    public void run() {
        LinkedList<Integer> memory = new LinkedList<>();
        Set<Integer> memorySet = new HashSet<>();

        int pageFaultCount = 0;

        for (int page : sequence) {
            if (!memorySet.contains(page)) {
                pageFaultCount++;

                if (memory.size() == maxMemoryFrames) {
                    Integer leastRecentlyUsed = memory.pollFirst();
                    if (leastRecentlyUsed != null) {
                        memorySet.remove(leastRecentlyUsed);
                    }
                }

                memory.addLast(page);
                memorySet.add(page);
            } else {
                memory.remove((Integer) page);
                memory.addLast(page);
            }
        }

        pageFaults[maxMemoryFrames] = pageFaultCount;
    }
}
