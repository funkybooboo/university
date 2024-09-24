package shell.commands.shellCommands;

import shell.commands.Command;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Path;
import java.nio.file.Paths;

public class Cd extends Command {
    public static final String NAME = "cd";

    public Cd(String[] commandParts) {
        super(commandParts);
    }

    @Override
    public OutputStream execute(InputStream inputStream) throws Exception {
        if (commandParts.length > 2) {
            throw new Exception("nash: " + NAME + ": too many arguments");
        }

        String currentDirectoryPath = System.getProperty("user.dir");
        Path targetDirectoryPath;

        if (commandParts.length == 1) {
            targetDirectoryPath = Paths.get(System.getProperty("user.home"));
        }
        else {
            targetDirectoryPath = Paths.get(commandParts[1]);
            if (!targetDirectoryPath.isAbsolute()) {
                targetDirectoryPath = Paths.get(currentDirectoryPath).resolve(targetDirectoryPath).normalize();
            }
        }

        if (!targetDirectoryPath.toFile().exists()) {
            throw new Exception("nash: " + NAME + ": the directory does not exist");
        }

        if (!targetDirectoryPath.toFile().isDirectory()) {
            throw new Exception("nash: " + NAME + ": the directory is not a directory");
        }

        System.setProperty("user.dir", targetDirectoryPath.toString());

        return new ByteArrayOutputStream();
    }
}
