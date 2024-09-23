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

    public String getName() {
        return NAME;
    }

    @Override
    public OutputStream execute(InputStream inputStream) throws Exception {

        // TODO fix the path displayed on the prompt (cd)

        if (commandParts.length > 2) {
            throw new Exception("nash: cd: too many arguments");
        }

        String currentDirectoryPath = System.getProperty("user.dir");
        Path targetDirectoryPath;

        if (commandParts.length == 1) {
            targetDirectoryPath = Paths.get(System.getProperty("user.home"));
        } else {
            targetDirectoryPath = Paths.get(commandParts[1]);
            if (!targetDirectoryPath.isAbsolute()) {
                targetDirectoryPath = Paths.get(currentDirectoryPath, targetDirectoryPath.toString());
            }
        }

        if (!targetDirectoryPath.toFile().exists()) {
            throw new Exception("nash: cd: the directory does not exist");
        }

        if (!targetDirectoryPath.toFile().isDirectory()) {
            throw new Exception("nash: cd: the directory is not a directory");
        }

        System.setProperty("user.dir", targetDirectoryPath.toString());

        return new ByteArrayOutputStream();
    }
}
