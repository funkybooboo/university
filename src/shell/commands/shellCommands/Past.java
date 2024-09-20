package shell.commands.shellCommands;

import shell.commands.Command;

public class Past implements Command {
    public static String name = "^";
    // If the user enters the command '^ 10', for example, the 10th command in the history buffer should be executed (a 1 based index);
    // there is a space between the ^ character and the number.  This command also goes into the history.
    // If the user then selects that command to be executed from history, the command it refers to should be executed,
    // for as deep as the history execution chain indicates.
    @Override
    public void execute(String[] arguments) {
        if (arguments.length != 1) {
            System.err.println("nash: ^: too many args for ^ command");
            return;
        }

        try {
            int index = Integer.parseInt(arguments[0]) - 1;
            String input = History.getCommand(index);

            if (input == null) {
                System.err.println("nash: ^: no such command in history");
            }
        } catch (NumberFormatException e) {
            System.err.println("nash: ^: invalid index format");
        } catch (Exception e) {
            System.err.println("nash: ^: unexpected error");
        }
    }
}
