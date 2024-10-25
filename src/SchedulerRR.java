import java.util.LinkedList;
import java.util.Queue;

public class SchedulerRR extends Scheduler {
    private final Queue<Process> readyQueue;
    private final Logger logger;
    private final int maxQuantum;
    private int quantum = 1;

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
            quantum = 1;
            return null;
        }

        if (currentProcess == null) {
            quantum = 1;
            currentProcess = readyQueue.poll();
            logger.log("CPU "+cpu+" > Scheduled "+currentProcess.getName());
            return currentProcess;
        }

        if (currentProcess.isBurstComplete()) {
            quantum = 1;
            logger.log("CPU "+cpu+" > Process "+currentProcess.getName()+" burst complete");
            if (currentProcess.isExecutionComplete()) {
                logger.log("CPU "+cpu+" > Process "+currentProcess.getName()+" execution complete");
            }
            else {
                readyQueue.add(currentProcess);
            }
            return switchProcesses(currentProcess, cpu);
        }

        if (quantum >= maxQuantum) {
            quantum = 1;
            readyQueue.add(currentProcess);
            logger.log("CPU "+cpu+" > Time quantum complete for process "+currentProcess.getName());
            return switchProcesses(currentProcess, cpu);
        }
        
        quantum += 1;
        return currentProcess;
    }

    private Process switchProcesses(Process currentProcess, int cpu) {
        Process nextProcess = readyQueue.poll();
        if (nextProcess == null) {
            return null;
        }
        if (nextProcess != currentProcess) {
            contextSwitches++;
        }
        currentProcess = nextProcess;
        logger.log("CPU "+ cpu +" > Scheduled "+ currentProcess.getName());
        return currentProcess;
    }
}
