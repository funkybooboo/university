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
    public OutputStream execute(InputStream inputStream) throws Exception {
        if (commandParts.length == 1) {
            throw new Exception("nash: " + NAME + ": missing operand");
        }

        String currentDirectoryPath = System.getProperty("user.dir");

        for (String directoryName : Arrays.copyOfRange(commandParts, 1, commandParts.length)) {
            File targetDirectoryPath = new File(directoryName);

            // Handle relative paths
            if (!targetDirectoryPath.isAbsolute()) {
                targetDirectoryPath = new File(currentDirectoryPath, directoryName);
            }

            if (targetDirectoryPath.exists()) {
                if (targetDirectoryPath.isDirectory()) {
                    throw new Exception("nash: " + NAME + ": " + directoryName + " directory already exists");
                }
                else {
                    throw new Exception("nash: " + NAME + ": " + directoryName + " file already exists");
                }
            }
            else if (!targetDirectoryPath.mkdir()) {
                throw new Exception("nash: " + NAME + ": failed to create directory: " + directoryName);
            }
        }
        return new ByteArrayOutputStream();
    }
}
