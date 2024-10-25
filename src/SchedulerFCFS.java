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
        if (currentProcess == null && readyQueue.isEmpty()) {
            return null;
        }

        if (currentProcess == null) {
            currentProcess = readyQueue.poll();
            logger.log("CPU "+cpu+" > Scheduled "+currentProcess.getName());
            return currentProcess;
        }

        if (currentProcess.isBurstComplete()) {
            logger.log("CPU "+cpu+" > Process "+currentProcess.getName()+" burst complete");
            if (currentProcess.isExecutionComplete()) {
                logger.log("CPU "+cpu+" > Process "+currentProcess.getName()+" execution complete");
            }
            else {
                readyQueue.add(currentProcess);
            }
            Process nextProcess = readyQueue.poll();
            if (nextProcess == null) {
                return null;
            }
            if (nextProcess != currentProcess) {
                contextSwitches++;
            }
            currentProcess = nextProcess;
            logger.log("CPU "+cpu+" > Scheduled "+currentProcess.getName());
        }

        return currentProcess;
    }
}
