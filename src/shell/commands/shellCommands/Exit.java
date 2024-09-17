package shell.commands.shellCommands;

import shell.commands.Command;
import shell.commands.Result;

public class Exit implements Command {
    public static String name = "exit";
    // The command 'exit' is used to terminate your shell program, when entered, your shell program should end.
    @Override
    public Result execute(String[] arguments, String previousOutput) {
        System.exit(0);
        return new Result();
    }
}
