package shell.commands.shellCommands;

import shell.commands.Command;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;

public class Mdir extends Command {
    public static final String NAME = "mdir";

    public Mdir(String[] commandParts) {
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

            // Check if the directory or file already exists
            if (targetDirectoryPath.exists()) {
                if (targetDirectoryPath.isDirectory()) {
                    throw new Exception("nash: " + NAME + ": " + directoryName + " directory already exists");
                }
                else {
                    throw new Exception("nash: " + NAME + ": " + directoryName + " file already exists");
                }
            }

            // Attempt to create the directory
            if (!targetDirectoryPath.mkdir()) {
                throw new Exception("nash: " + NAME + ": failed to create directory: " + directoryName);
            }
        }
        return new ByteArrayOutputStream();
    }
}
