package shell.commands.shellCommands;

import shell.commands.Command;

import java.nio.file.Path;
import java.nio.file.Paths;

public class Cd implements Command {
    public static String name = "cd";

    @Override
    public void execute(String[] arguments) {
        if (arguments.length > 1) {
            System.err.println("nash: cd: too many arguments");
            return;
        }

        String currentDirectoryPath = System.getProperty("user.dir");
        Path targetDirectoryPath;

        if (arguments.length == 0) {
            targetDirectoryPath = Paths.get(System.getProperty("user.home"));
        } else {
            targetDirectoryPath = Paths.get(arguments[0]);
            if (!targetDirectoryPath.isAbsolute()) {
                targetDirectoryPath = Paths.get(currentDirectoryPath, targetDirectoryPath.toString());
            }
        }

        if (!targetDirectoryPath.toFile().exists()) {
            System.err.println("nash: cd: the directory does not exist");
            return;
        }

        if (!targetDirectoryPath.toFile().isDirectory()) {
            System.err.println("nash: cd: the directory is not a directory");
            return;
        }

        System.setProperty("user.dir", targetDirectoryPath.toString());
    }
}
