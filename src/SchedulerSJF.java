import java.util.Comparator;
import java.util.PriorityQueue;

public class SchedulerSJF extends Scheduler {
    private final PriorityQueue<Process> readyQueue;
    private final Logger logger;

    public SchedulerSJF(Logger logger) {
        this.logger = logger;
        this.readyQueue = new PriorityQueue<>(Comparator.comparingInt(Process::getBurstTime));
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
