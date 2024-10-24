import java.util.Comparator;
import java.util.PriorityQueue;

public class SchedulerPriority extends Scheduler {
    private final PriorityQueue<Process> readyQueue;
    private final Logger logger;

    public SchedulerPriority(Logger logger) {
        this.logger = logger;
        this.readyQueue = new PriorityQueue<>(Comparator.comparingInt(Process::getPriority));
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
