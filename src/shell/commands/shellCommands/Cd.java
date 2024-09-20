package shell.commands.shellCommands;

import shell.commands.Command;
import shell.commands.Result;

import java.nio.file.Path;
import java.nio.file.Paths;

public class Cd implements Command {
    public static String name = "cd";

    @Override
    public Result execute(String[] arguments, String previousOutput) {
        if (arguments.length > 1) {
            return new Result("nash: cd: too many arguments", false);
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
            return new Result("nash: cd: the directory does not exist", false);
        }

        if (!targetDirectoryPath.toFile().isDirectory()) {
            return new Result("nash: cd: the directory is not a directory", false);
        }

        System.setProperty("user.dir", targetDirectoryPath.toString());
        return new Result();
    }
}
