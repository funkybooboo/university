import java.util.Comparator;
import java.util.PriorityQueue;

/**
 * SchedulerSJF implements the Shortest Job First (SJF) scheduling algorithm.
 * It manages a priority queue of processes and schedules them based on their total execution time.
 *
 * @author Nate Stott
 */
public class SchedulerSJF extends Scheduler {
    private final PriorityQueue<Process> readyQueue; // Priority queue to hold processes sorted by total execution time
    private final Logger logger; // Logger to record scheduling events

    /**
     * Constructs a SchedulerSJF instance with a specified logger.
     *
     * @param logger the logger used for logging scheduling events
     */
    public SchedulerSJF(Logger logger) {
        this.logger = logger;
        this.readyQueue = new PriorityQueue<>(Comparator.comparingInt(Process::getTotalTime));
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
     * Updates the current scheduled process based on the scheduling rules.
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
            currentProcess = readyQueue.poll(); // Get the next process from the queue
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
            Process nextProcess = readyQueue.poll(); // Get the next process from the queue
            if (nextProcess == null) {
                contextSwitches++; // One context switch for going from no process running to a process running
                return null; // No process to schedule
            }
            if (nextProcess != currentProcess) {
                contextSwitches += 2; // Two context switches for switching processes
            }
            currentProcess = nextProcess; // Switch to the next process
            logger.log("CPU " + cpu + " > Scheduled " + currentProcess.getName());
        }

        return currentProcess; // Return the process that has been scheduled
    }
}
