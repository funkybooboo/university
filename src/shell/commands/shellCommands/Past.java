package shell.commands.shellCommands;

import shell.commands.Command;
import shell.commands.Result;

public class Past implements Command {
    public static String name = "^";
    // If the user enters the command '^ 10', for example, the 10th command in the history buffer should be executed (a 1 based index);
    // there is a space between the ^ character and the number.  This command also goes into the history.
    // If the user then selects that command to be executed from history, the command it refers to should be executed,
    // for as deep as the history execution chain indicates.
    @Override
    public Result execute(String[] arguments, String previousOutput) {
        if (arguments.length != 1) {
            return new Result("nash: ^: too many args for ^ command", false);
        }

        try {
            int index = Integer.parseInt(arguments[0]) - 1;
            String input = History.getCommand(index);

            if (input == null) {
                return new Result("nash: ^: no such command in history", false);
            }

            return new Result(input, true, true);
        } catch (NumberFormatException e) {
            return new Result("nash: ^: invalid index format", false);
        } catch (Exception e) {
            return new Result("nash: ^: unexpected error", false);
        }
    }
}
