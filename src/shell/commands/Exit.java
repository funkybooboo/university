package shell.commands;

import java.io.File;

public class Exit implements Command {
    static String name = "exit";
    // The command 'exit' is used to terminate your shell program, when entered, your shell program should end.
    @Override
    public Result execute(String[] arguments) {
        if (arguments.length > 1) {
            return new Result("nash: cd: too many arguments", false);
        }

        File newDirectory;
        if (arguments.length == 0) {
            newDirectory = new File(System.getProperty("user.home"));
        } else {
            newDirectory = new File(arguments[0]);
        }

        if (newDirectory.exists() && newDirectory.isDirectory()) {
            System.setProperty("user.dir", newDirectory.getAbsolutePath());
            return new Result();
        } else {
            return new Result("nash: cd: the directory does not exist or is not a directory", false);
        }
    }
}
