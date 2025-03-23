import java.util.Comparator;
import java.util.PriorityQueue;

/**
 * SchedulerPriority implements a priority-based scheduling algorithm.
 * It manages a priority queue of processes that are ready to be executed
 * based on their priority level.
 *
 * @author Nate Stott
 */
public class SchedulerPriority extends Scheduler {
    private final PriorityQueue<Process> readyQueue; // Priority queue to hold processes sorted by priority
    private final Logger logger; // Logger to record scheduling events

    /**
     * Constructs a SchedulerPriority instance with a specified logger.
     *
     * @param logger the logger used for logging scheduling events
     */
    public SchedulerPriority(Logger logger) {
        this.logger = logger;
        this.readyQueue = new PriorityQueue<>(Comparator.comparingInt(Process::getPriority));
    }

    /**
     * Notifies the scheduler of a new process to be added to the ready queue.
     *
     * @param process the new process to add to the queue
     */
    @Override
    void notifyNewProcess(Process process) {
        readyQueue.add(process);
    }

    /**
     * Updates the current process based on the scheduling rules.
     *
     * @param currentProcess the currently executing process
     * @param cpu the current CPU the process is running on
     * @return the process that is scheduled to execute on the cpu
     */
    @Override
    Process update(Process currentProcess, int cpu) {
        if (currentProcess == null && readyQueue.isEmpty()) {
            return null; // No processes to schedule
        }

        if (currentProcess == null) {
            currentProcess = readyQueue.poll(); // Get the next process in the priority queue
            if (currentProcess != null) {
                logger.log("CPU " + cpu + " > Scheduled " + currentProcess.getName());
                contextSwitches++; // One context switch for going from no process running to a process running
            }
            return currentProcess;
        }

        if (currentProcess.isBurstComplete()) {
            logger.log("CPU " + cpu + " > Process " + currentProcess.getName() + " burst complete");
            if (currentProcess.isExecutionComplete()) {
                logger.log("CPU " + cpu + " > Process " + currentProcess.getName() + " execution complete");
            } else {
                readyQueue.add(currentProcess); // Re-add the process to the queue if not complete
            }
            Process nextProcess = readyQueue.poll(); // Get the next process in the priority queue
            if (nextProcess == null) {
                contextSwitches++; // One context switch for going from no process running to a process running
                return null;
            }
            if (nextProcess != currentProcess) {
                contextSwitches += 2; // Two context switches for switching processes
            }
            currentProcess = nextProcess;
            logger.log("CPU " + cpu + " > Scheduled " + currentProcess.getName());
        }

        return currentProcess; // Return the process that has been scheduled
    }
}
