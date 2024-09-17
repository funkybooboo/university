package shell.commands.shellCommands;

import shell.commands.Command;
import shell.commands.Result;

public class Exit implements Command {
    public static String name = "exit";
    // The command 'exit' is used to terminate your shell program, when entered, your shell program should end.
    @Override
    public Result execute(String[] arguments, String previousOutput) {
        if (arguments.length == 0) {
            System.exit(0);
        }
        if (arguments.length == 1) {
            try {
                System.exit(Integer.parseInt(arguments[0]));
            }
            catch (Exception e) {
                return new Result("nash: exit: invalid exit code", false);
            }
        }
        else {
            return new Result("nash: exit: too many arguments", false);
        }

        return new Result();
    }
}
