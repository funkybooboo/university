import java.util.Comparator;
import java.util.PriorityQueue;

/**
 * SchedulerSRTF implements the Shortest Remaining Time First (SRTF) scheduling algorithm.
 * It manages a priority queue of processes and schedules them based on their remaining execution time.
 *
 * @author Nate Stott
 */
public class SchedulerSRTF extends Scheduler {
    private final PriorityQueue<Process> readyQueue; // Priority queue to hold processes sorted by remaining time
    private final Logger logger; // Logger to record scheduling events

    /**
     * Constructs a SchedulerSRTF instance with a specified logger.
     *
     * @param logger the logger used for logging scheduling events
     */
    public SchedulerSRTF(Logger logger) {
        this.logger = logger;
        this.readyQueue = new PriorityQueue<>(
                Comparator.comparingInt(a -> a.getTotalTime() - a.getElapsedTotal()) // Sort by remaining time
        );
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
            }
        }

        if (!currentProcess.isExecutionComplete()) {
            readyQueue.add(currentProcess); // Re-add the current process if not complete
        }

        Process nextProcess = readyQueue.poll(); // Get the next process from the queue
        if (nextProcess == null) {
            contextSwitches++; // One context switch for going from no process running to a process running
            return null; // No process to schedule
        }

        if (nextProcess != currentProcess) {
            if (!currentProcess.isExecutionComplete()) {
                logger.log("CPU " + cpu + " > Preemptively removed: " + currentProcess.getName());
            }
            currentProcess = nextProcess; // Switch to the next process
            logger.log("CPU " + cpu + " > Scheduled " + currentProcess.getName());
            contextSwitches += 2; // Two context switches for switching processes
        }

        return currentProcess; // Return the process that has been scheduled
    }
}
