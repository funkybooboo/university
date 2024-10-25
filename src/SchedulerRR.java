import java.util.LinkedList;
import java.util.Queue;

public class SchedulerRR extends Scheduler {
    private final Queue<Process> readyQueue;
    private final Logger logger;
    private final int maxQuantum;
    private int quantum;

    public SchedulerRR(Logger logger, int quantum) {
        this.logger = logger;
        this.maxQuantum = quantum;
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
            return currentProcess;
        }

        if (quantum >= maxQuantum) {
            quantum = 0;
            readyQueue.add(currentProcess);
            logger.log("CPU "+cpu+" > Time quantum complete for process "+currentProcess.getName());
            Process nextProcess = readyQueue.poll();
            if (nextProcess == null) {
                return null;
            }
            if (nextProcess != currentProcess) {
                contextSwitches++;
            }
            currentProcess = nextProcess;
            logger.log("CPU "+cpu+" > Scheduled "+currentProcess.getName());
            return currentProcess;
        }
        
        quantum += 1;
        return currentProcess;
    }
}
