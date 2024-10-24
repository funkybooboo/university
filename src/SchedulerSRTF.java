import java.util.Comparator;
import java.util.PriorityQueue;

public class SchedulerSRTF extends Scheduler {
    private final PriorityQueue<Process> readyQueue;
    private final Logger logger;

    public SchedulerSRTF(Logger logger) {
        this.logger = logger;
        this.readyQueue = new PriorityQueue<>(Comparator.comparingInt(Process::getRemainingBurst));
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
