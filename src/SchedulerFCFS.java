import java.util.LinkedList;
import java.util.Queue;

public class SchedulerFCFS extends Scheduler {
    private final Queue<Process> readyQueue;
    private final Logger logger;

    public SchedulerFCFS(Logger logger) {
        this.logger = logger;
        this.readyQueue = new LinkedList<>();
    }

    @Override
    void notifyNewProcess(Process process) {
        readyQueue.add(process);
    }

    @Override
    Process update(Process currentProcess, int cpu) {
        return null;
    }
}
