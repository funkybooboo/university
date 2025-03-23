package shell.commands.shellCommands;

import shell.commands.Command;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * The Cd class represents a command to change the current working directory.
 * It extends the Command class and implements the execute method.
 *
 * @author Nate Stott
 */
public class Cd extends Command {
    public static final String NAME = "cd"; // Command name for changing directories

    /**
     * Constructs a Cd command with the specified command parts.
     *
     * @param commandParts The parts of the command.
     */
    public Cd(String[] commandParts) {
        super(commandParts);
    }

    /**
     * Executes the Cd command to change the current working directory.
     *
     * @param inputStream The input stream (not used in this command).
     * @param commandIndex The index of the command in a pipeline.
     * @param commandsLength The total number of commands in the pipeline.
     * @return An empty output stream (not used).
     * @throws Exception If the command arguments are invalid or if the directory change fails.
     */
    @Override
    public OutputStream execute(InputStream inputStream, int commandIndex, int commandsLength) throws Exception {
        if (commandParts.length > 2) {
            throw new Exception("nash: " + NAME + ": too many arguments"); // Validate argument count
        }

        String currentDirectoryPath = System.getProperty("user.dir");
        Path targetDirectoryPath;

        // Default to user's home directory if no argument is provided
        if (commandParts.length == 1) {
            targetDirectoryPath = Paths.get(System.getProperty("user.home"));
        } else {
            targetDirectoryPath = Paths.get(commandParts[1]);
            // Resolve relative paths against the current directory
            if (!targetDirectoryPath.isAbsolute()) {
                targetDirectoryPath = Paths.get(currentDirectoryPath).resolve(targetDirectoryPath).normalize();
            }
        }

        // Check if the target directory exists
        if (!targetDirectoryPath.toFile().exists()) {
            throw new Exception("nash: " + NAME + ": the directory does not exist");
        }

        // Check if the target is a directory
        if (!targetDirectoryPath.toFile().isDirectory()) {
            throw new Exception("nash: " + NAME + ": the directory is not a directory");
        }

        // Change the current working directory
        System.setProperty("user.dir", targetDirectoryPath.toString());

        return new ByteArrayOutputStream(); // Return empty stream (not used)
    }
}
