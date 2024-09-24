package shell.commands.shellCommands;

import shell.commands.Command;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.Objects;

public class Rdir extends Command {
    public static final String NAME = "rdir";

    public Rdir(String[] commandParts) {
        super(commandParts);
    }

    @Override
    public OutputStream execute(InputStream inputStream, int commandIndex, int commandsLength) throws Exception {
        if (commandParts.length == 1) {
            throw new Exception("nash: " + NAME + ": missing operand");
        }

        String currentDirectoryPath = System.getProperty("user.dir");

        for (String directoryName : Arrays.copyOfRange(commandParts, 1, commandParts.length)) {
            File targetDirectoryPath;

            // Handle relative paths
            if (new File(directoryName).isAbsolute()) {
                targetDirectoryPath = new File(directoryName).getCanonicalFile();
            }
            else {
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
                }
                else {
                    throw new Exception("nash: " + NAME + ": " + directoryName + " is not a directory");
                }
            }
            else {
                throw new Exception("nash: " + NAME + ": directory " + directoryName + " does not exist");
            }
        }
        return new ByteArrayOutputStream();
    }
}
