package shell.commands.shellCommands;

import shell.commands.Command;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * The List class represents a command for listing files and directories.
 * It extends the Command class and implements the execute method.
 *
 * @author Nate Stott
 */
public class List extends Command {
    public static final String NAME = "list"; // Command name for listing files

    /**
     * Constructs a List command with the specified command parts.
     *
     * @param commandParts The parts of the command.
     */
    public List(String[] commandParts) {
        super(commandParts);
    }

    /**
     * Executes the List command to list files and directories.
     *
     * @param inputStream The input stream (not used in this command).
     * @param commandIndex The index of the command in a pipeline.
     * @param commandsLength The total number of commands in the pipeline.
     * @return An output stream containing the list of files and directories.
     * @throws Exception If an error occurs during execution.
     */
    @Override
    public OutputStream execute(InputStream inputStream, int commandIndex, int commandsLength) throws Exception {
        if (commandParts.length > 2) {
            throw new Exception("nash: " + NAME + ": invalid number of arguments"); // Validate number of arguments
        }

        // Determine the starting directory
        String start = (commandParts.length == 1) ? "." : commandParts[1];
        File targetDirectoryPath;

        // Resolve the path
        if (!new File(start).isAbsolute()) {
            String currentDirectoryPath = System.getProperty("user.dir");
            targetDirectoryPath = new File(currentDirectoryPath, start).getCanonicalFile();
        } else {
            targetDirectoryPath = new File(start).getCanonicalFile();
        }

        // Check if the directory exists and is a directory
        if (!targetDirectoryPath.exists() || !targetDirectoryPath.isDirectory()) {
            throw new Exception("nash: " + NAME + ": no such file or directory"); // Directory not found
        }

        // List all files and directories in the target directory
        File[] files = targetDirectoryPath.listFiles();
        if (files == null) {
            throw new Exception("nash: " + NAME + ": unable to access the directory"); // Access denied or error
        }

        // Prepare output
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        for (File file : files) {
            String permissions = getPermissions(file);
            String size = String.format("%10d", file.length());
            String modificationDate = getModificationDate(file);
            String name = file.getName();
            output.write(String.format("%s %s %s %s%n", permissions, size, modificationDate, name).getBytes());
        }
        return output; // Return the output stream
    }

    /**
     * Gets the permissions of a file in a string format.
     *
     * @param file The file to check permissions for.
     * @return A string representing the file's permissions.
     */
    private String getPermissions(File file) {
        return String.valueOf(file.isDirectory() ? 'd' : '-') +
                (file.canRead() ? 'r' : '-') +
                (file.canWrite() ? 'w' : '-') +
                (file.canExecute() ? 'x' : '-');
    }

    /**
     * Gets the last modification date of a file.
     *
     * @param file The file to check the modification date for.
     * @return A formatted string representing the last modification date.
     */
    private String getModificationDate(File file) {
        SimpleDateFormat dateFormat = new SimpleDateFormat("MMM dd yyyy hh:mm");
        Date lastModified = new Date(file.lastModified());
        return dateFormat.format(lastModified); // Format date to specified pattern
    }
}
