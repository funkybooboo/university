package shell.commands.shellCommands;

import shell.commands.Command;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * The Exit class represents a command to terminate the shell program.
 * It extends the Command class and implements the execute method.
 *
 * @author Nate Stott
 */
public class Exit extends Command {
    public static final String NAME = "exit"; // Command name for exiting the shell

    /**
     * Constructs an Exit command with the specified command parts.
     *
     * @param commandParts The parts of the command.
     */
    public Exit(String[] commandParts) {
        super(commandParts);
    }

    /**
     * Executes the Exit command to terminate the shell program.
     *
     * @param inputStream The input stream (not used in this command).
     * @param commandIndex The index of the command in a pipeline.
     * @param commandsLength The total number of commands in the pipeline.
     * @return An empty output stream (not used).
     * @throws Exception If the command arguments are invalid.
     */
    @Override
    public OutputStream execute(InputStream inputStream, int commandIndex, int commandsLength) throws Exception {
        if (commandParts.length == 1) {
            System.exit(0); // Exit with status code 0
        }
        if (commandParts.length == 2) {
            try {
                System.exit(Integer.parseInt(commandParts[1])); // Exit with provided status code
            } catch (NumberFormatException e) {
                throw new Exception("nash: " + NAME + ": invalid exit code"); // Handle invalid exit code format
            }
        } else {
            throw new Exception("nash: " + NAME + ": too many arguments"); // Validate number of arguments
        }
        return new ByteArrayOutputStream(); // Return empty stream (not used)
    }
}
