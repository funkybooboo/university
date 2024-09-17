package shell.commands.shellCommands;

import shell.commands.Command;
import shell.commands.Result;

import java.io.File;

public class Cd implements Command {
    public static String name = "cd";
    // If the user enters the command "cd" without any parameters, the shell should change the working directory to the user's home folder.
    // The shell should verify the directory the user attempts to change to exists, reporting an error if it isn't.
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
            System.setProperty("user.dir", targetDirectory.getAbsolutePath());
            return new Result();
        } else {
            String errorMessage = (arguments.length == 0)
                    ? "nash: cd: home directory does not exist"
                    : "nash: cd: the directory does not exist or is not a directory";
            return new Result(errorMessage, false);
        }
    }

}
