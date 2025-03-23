package shell.commands.shellCommands;

import shell.commands.Command;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * The Ptime class represents a command that reports the cumulative time
 * spent in child processes. It extends the Command class and implements
 * the execute method.
 *
 * @author Nate Stott
 */
public class Ptime extends Command {
    public static final String NAME = "ptime"; // Command name
    private static double cumulativeTime = 0.0; // Cumulative time in seconds

    /**
     * Constructs a Ptime command with the specified command parts.
     *
     * @param commandParts The parts of the command.
     */
    public Ptime(String[] commandParts) {
        super(commandParts);
    }

    /**
     * Updates the cumulative time with the specified duration.
     *
     * @param time The time to add to the cumulative total.
     */
    public static void updateCumulativeTime(double time) {
        cumulativeTime += time; // Add time to cumulative total
    }

    /**
     * Executes the Ptime command to display the total cumulative time.
     *
     * @param inputStream The input stream (not used in this command).
     * @param commandIndex The index of the command in a pipeline.
     * @param commandsLength The total number of commands in the pipeline.
     * @return An output stream containing the cumulative time message.
     * @throws Exception If an error occurs during execution.
     */
    @Override
    public OutputStream execute(InputStream inputStream, int commandIndex, int commandsLength) throws Exception {
        if (commandParts.length > 1) {
            throw new Exception("nash: " + NAME + ": too many arguments\n"); // Check for too many arguments
        }

        // Format the cumulative time message
        String message = String.format("Total time in child processes: %.4f seconds%n", cumulativeTime);
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        output.write(message.getBytes()); // Write message to output stream
        return output; // Return the output stream with the message
    }
}
