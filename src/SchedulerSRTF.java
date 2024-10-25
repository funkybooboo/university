import java.util.Comparator;
import java.util.PriorityQueue;

public class SchedulerSRTF extends Scheduler {
    private final PriorityQueue<Process> readyQueue;
    private final Logger logger;

    public SchedulerSRTF(Logger logger) {
        this.logger = logger;
        this.readyQueue = new PriorityQueue<>(
                Comparator.comparingInt(a -> a.getTotalTime() - a.getElapsedTotal())
        );
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
        }

        if (!currentProcess.isExecutionComplete()) {
            readyQueue.add(currentProcess);
        }
        Process nextProcess = readyQueue.poll();
        if (nextProcess == null) {
            return null;
        }
        if (nextProcess != currentProcess) {
            if (!currentProcess.isExecutionComplete()) {
                logger.log("CPU "+cpu+" > Preemptively removed: "+currentProcess.getName());
            }
            currentProcess = nextProcess;
            logger.log("CPU "+cpu+" > Scheduled "+currentProcess.getName());
            contextSwitches++;
        }

        return currentProcess;
    }
}
