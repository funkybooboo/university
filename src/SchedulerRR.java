import java.util.LinkedList;
import java.util.Queue;

public class SchedulerRR extends Scheduler {
    private final Queue<Process> readyQueue;
    private final Logger logger;
    private final int quantum;

    public SchedulerRR(Logger logger, int quantum) {
        this.logger = logger;
        this.quantum = quantum;
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
