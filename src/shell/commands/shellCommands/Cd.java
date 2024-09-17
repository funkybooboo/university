package shell.commands.shellCommands;

import shell.commands.Command;
import shell.commands.Result;

import java.io.File;

public class Cd implements Command {
    public static String name = "cd";

    @Override
    public Result execute(String[] arguments, String previousOutput) {
        if (arguments.length > 1) {
            return new Result("nash: cd: too many arguments", false);
        }

        File targetDirectory;
        if (arguments.length == 0) {
            targetDirectory = new File(System.getProperty("user.home"));
        } else {
            targetDirectory = new File(arguments[0]);
        }

        if (targetDirectory.exists() && targetDirectory.isDirectory()) {
            try {
                System.setProperty("user.dir", targetDirectory.getAbsolutePath());
            } catch (SecurityException e) {
                return new Result("nash: cd: permission denied", false);
            }
            return new Result();
        } else {
            String errorMessage = (arguments.length == 0)
                    ? "nash: cd: home directory does not exist"
                    : "nash: cd: the directory does not exist or is not a directory";
            return new Result(errorMessage, false);
        }
    }
}
