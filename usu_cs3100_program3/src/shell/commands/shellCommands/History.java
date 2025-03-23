package shell.commands.shellCommands;

import shell.commands.Command;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * The History class represents a command for viewing and managing the command history.
 * It extends the Command class and implements the execute method.
 *
 * @author Nate Stott
 */
public class History extends Command {
    public static final String NAME = "history"; // Command name for viewing history
    private static final List<String> commandHistory = new ArrayList<>(); // List to store command history

    /**
     * Constructs a History command with the specified command parts.
     *
     * @param commandParts The parts of the command.
     */
    public History(String[] commandParts) {
        super(commandParts);
    }

    /**
     * Retrieves a command from the history based on the given index.
     *
     * @param index The index of the command in history.
     * @return The command string at the specified index.
     */
    public static String getCommand(int index) {
        return commandHistory.get(index);
    }

    /**
     * Adds a command to the history.
     *
     * @param command The command to be added to history.
     */
    public static void addCommand(String command) {
        commandHistory.add(command);
    }

    /**
     * Returns the total number of commands in history.
     *
     * @return The size of the command history.
     */
    public static int size() {
        return commandHistory.size();
    }

    /**
     * Executes the History command to list all previous commands.
     *
     * @param inputStream The input stream (not used in this command).
     * @param commandIndex The index of the command in a pipeline.
     * @param commandsLength The total number of commands in the pipeline.
     * @return An output stream containing the command history.
     * @throws Exception If an error occurs during execution.
     */
    @Override
    public OutputStream execute(InputStream inputStream, int commandIndex, int commandsLength) throws Exception {
        if (commandParts.length > 1) {
            throw new Exception("nash: " + NAME + ": too many args for history command"); // Validate number of arguments
        }

        ByteArrayOutputStream output = new ByteArrayOutputStream();
        output.write("-- Command History --\n".getBytes()); // Header for history output

        for (int i = 0; i < commandHistory.size(); i++) {
            output.write(((i + 1) + " : " + commandHistory.get(i) + "\n").getBytes()); // Format and write each command
        }

        return output; // Return the output stream containing history
    }
}
