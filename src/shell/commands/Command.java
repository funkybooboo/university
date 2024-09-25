package shell.commands;

import java.io.InputStream;
import java.io.OutputStream;

/**
 * The Command class serves as an abstract base class for all command types.
 * It defines the structure that all specific command implementations must follow.
 *
 * @author Nate Stott
 */
public abstract class Command {

    protected final String[] commandParts; // Parts of the command to be executed

    /**
     * Constructs a Command with the specified command parts.
     *
     * @param commandParts The parts of the command.
     */
    public Command(String[] commandParts) {
        this.commandParts = commandParts; // Initialize command parts
    }

    /**
     * Executes the command with the given input stream.
     *
     * @param inputStream The input stream to provide input to the command.
     * @param commandIndex The index of the command in a pipeline.
     * @param commandsLength The total number of commands in the pipeline.
     * @return An output stream containing the command's output.
     * @throws Exception If an error occurs during execution.
     */
    public abstract OutputStream execute(InputStream inputStream, int commandIndex, int commandsLength) throws Exception;
}
