import java.util.LinkedList;
import java.util.Queue;

/**
 * SchedulerRR implements a Round Robin (RR) scheduling algorithm.
 * It manages a queue of processes and uses a time quantum for process scheduling.
 *
 * @author Nate Stott
 */
public class SchedulerRR extends Scheduler {
    private final Queue<Process> readyQueue; // Queue to hold processes that are ready for execution
    private final Logger logger; // Logger to record scheduling events
    private final int maxQuantum; // Maximum time quantum for process execution
    private int quantum = 1; // Current time quantum for the executing process

    /**
     * Constructs a SchedulerRR instance with a specified logger and time quantum.
     *
     * @param logger the logger used for logging scheduling events
     * @param quantum the time quantum for scheduling processes
     */
    public SchedulerRR(Logger logger, int quantum) {
        this.logger = logger;
        this.maxQuantum = quantum;
        this.readyQueue = new LinkedList<>();
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
            quantum = 1; // Reset quantum for the next cycle
            return null; // No processes to schedule
        }

        if (currentProcess == null) {
            quantum = 1; // Reset quantum for a new process
            currentProcess = readyQueue.poll(); // Get the next process in the queue
            if (currentProcess != null) {
                logger.log("CPU " + cpu + " > Scheduled " + currentProcess.getName());
                contextSwitches++; // One context switch for going from no process running to a process running
            }
            return currentProcess;
        }

        if (currentProcess.isBurstComplete()) {
            quantum = 1; // Reset quantum when burst is complete
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
            return currentProcess; // Return the new current process
        }

        if (quantum >= maxQuantum) {
            quantum = 1; // Reset quantum after reaching max
            readyQueue.add(currentProcess); // Add the current process back to the queue
            logger.log("CPU " + cpu + " > Time quantum complete for process " + currentProcess.getName());
            Process nextProcess = readyQueue.poll(); // Get the next process from the queue
            if (nextProcess == null) {
                contextSwitches++; // One context switch for going from no process running to a process running
                return null; // No process to schedule
            }
            contextSwitches += 2; // Two context switches for switching processes
            currentProcess = nextProcess; // Switch to the next process
            logger.log("CPU " + cpu + " > Scheduled " + currentProcess.getName());
            return currentProcess; // Return the new current process
        }

        quantum++; // Increment the quantum for the current process
        return currentProcess; // Return the current process since still within quantum
    }
}
