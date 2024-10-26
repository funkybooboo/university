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
        if (currentProcess == null && readyQueue.isEmpty()) {
            return null;
        }

        if (currentProcess == null) {
            currentProcess = readyQueue.poll();
            logger.log("CPU "+cpu+" > Scheduled "+currentProcess.getName());
            contextSwitches++;
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
                contextSwitches++;
                return null;
            }
            if (nextProcess != currentProcess) {
                contextSwitches++;
                contextSwitches++;
            }
            currentProcess = nextProcess;
            logger.log("CPU "+cpu+" > Scheduled "+currentProcess.getName());
        }

        return currentProcess;
    }
}
