package shell.commands.shellCommands;

import shell.commands.Command;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.Objects;

/**
 * The Rdir class represents a command that removes empty directories.
 * It extends the Command class and implements the execute method.
 *
 * @author Nate Stott
 */
public class Rdir extends Command {
    public static final String NAME = "rdir"; // Command name

    /**
     * Constructs a Rdir command with the specified command parts.
     *
     * @param commandParts The parts of the command, including the directory names.
     */
    public Rdir(String[] commandParts) {
        super(commandParts);
    }

    /**
     * Executes the Rdir command to remove empty directories.
     *
     * @param inputStream The input stream (not used in this command).
     * @param commandIndex The index of the command in a pipeline.
     * @param commandsLength The total number of commands in the pipeline.
     * @return An output stream indicating the result of the operation.
     * @throws Exception If an error occurs during execution.
     */
    @Override
    public OutputStream execute(InputStream inputStream, int commandIndex, int commandsLength) throws Exception {
        if (commandParts.length == 1) {
            throw new Exception("nash: " + NAME + ": missing operand"); // Check for missing directory names
        }

        String currentDirectoryPath = System.getProperty("user.dir"); // Get current working directory

        // Iterate over each directory name provided in the command
        for (String directoryName : Arrays.copyOfRange(commandParts, 1, commandParts.length)) {
            File targetDirectoryPath;

            // Handle relative and absolute paths
            if (new File(directoryName).isAbsolute()) {
                targetDirectoryPath = new File(directoryName).getCanonicalFile();
            } else {
                targetDirectoryPath = new File(currentDirectoryPath, directoryName).getCanonicalFile();
            }

            // Check if the directory exists
            if (targetDirectoryPath.exists()) {
                if (targetDirectoryPath.isDirectory()) {
                    // Check if the directory is empty
                    if (Objects.requireNonNull(targetDirectoryPath.list()).length > 0) {
                        throw new Exception("nash: " + NAME + ": directory " + directoryName + " is not empty");
                    }

                    // Attempt to delete the empty directory
                    if (!targetDirectoryPath.delete()) {
                        throw new Exception("nash: " + NAME + ": failed to remove directory: " + directoryName);
                    }
                } else {
                    throw new Exception("nash: " + NAME + ": " + directoryName + " is not a directory");
                }
            } else {
                throw new Exception("nash: " + NAME + ": directory " + directoryName + " does not exist");
            }
        }
        return new ByteArrayOutputStream(); // Return an empty output stream
    }
}
