package shell.commands.shellCommands;

public class Past {
    public static final String NAME = "^";
    // If the user enters the command '^ 10', for example, the 10th command in the history buffer should be executed (a 1 based index);
    // there is a space between the ^ character and the number.  This command also goes into the history.
    // If the user then selects that command to be executed from history, the command it refers to should be executed,
    // for as deep as the history execution chain indicates.
    public static String execute(String[] arguments) {
        if (arguments.length != 1) {
            System.err.println("nash: ^: too many args for ^ command");
            return null;
        }

        try {
            int index = Integer.parseInt(arguments[0]) - 1;
            String input = History.getCommand(index);

            if (input == null) {
                System.err.println("nash: ^: no such command in history");
            }

            return input;

        } catch (NumberFormatException e) {
            System.err.println("nash: ^: invalid index format");
        } catch (Exception e) {
            System.err.println("nash: ^: unexpected error");
        }
        return null;
    }
}
