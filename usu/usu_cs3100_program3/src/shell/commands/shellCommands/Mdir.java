package shell.commands.shellCommands;

import shell.commands.Command;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;

/**
 * The Mdir class represents a command for creating new directories.
 * It extends the Command class and implements the execute method.
 *
 * @author Nate Stott
 */
public class Mdir extends Command {
    public static final String NAME = "mdir"; // Command name for making directories

    /**
     * Constructs a Mdir command with the specified command parts.
     *
     * @param commandParts The parts of the command.
     */
    public Mdir(String[] commandParts) {
        super(commandParts);
    }

    /**
     * Executes the Mdir command to create new directories.
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

        String currentDirectoryPath = System.getProperty("user.dir");

        for (String directoryName : Arrays.copyOfRange(commandParts, 1, commandParts.length)) {
            File targetDirectoryPath;

            // Handle relative and absolute paths
            if (new File(directoryName).isAbsolute()) {
                targetDirectoryPath = new File(directoryName).getCanonicalFile();
            } else {
                targetDirectoryPath = new File(currentDirectoryPath, directoryName).getCanonicalFile();
            }

            // Check if the directory or file already exists
            if (targetDirectoryPath.exists()) {
                if (targetDirectoryPath.isDirectory()) {
                    throw new Exception("nash: " + NAME + ": " + directoryName + " directory already exists"); // Directory exists
                } else {
                    throw new Exception("nash: " + NAME + ": " + directoryName + " file already exists"); // File exists
                }
            }

            // Attempt to create the directory
            if (!targetDirectoryPath.mkdir()) {
                throw new Exception("nash: " + NAME + ": failed to create directory: " + directoryName); // Creation failed
            }
        }
        return new ByteArrayOutputStream(); // Return an empty output stream
    }
}
