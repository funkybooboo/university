package shell.commands.shellCommands;

/**
 * The Past class represents a command that retrieves a previous command
 * from the history based on the provided index. It allows users to
 * execute commands from their command history.
 *
 * @author Nate Stott
 */
public class Past {
    public static final String NAME = "^"; // Command name for retrieving past commands

    /**
     * Executes the past command by retrieving a command from history
     * using the provided index.
     *
     * @param arguments An array containing the index of the command to retrieve.
     * @return The retrieved command if found, otherwise null.
     */
    public static String execute(String[] arguments) {
        if (arguments.length != 1) {
            System.err.println("nash: " + NAME + ": too many args for ^ command"); // Check for correct number of arguments
            return null;
        }

        try {
            // Parse the index from the arguments and adjust for zero-based indexing
            int index = Integer.parseInt(arguments[0]) - 1;
            String input = History.getCommand(index); // Retrieve the command from history

            if (input == null) {
                System.err.println("nash: " + NAME + ": no such command in history"); // Handle case where command does not exist
            }

            return input; // Return the retrieved command

        } catch (NumberFormatException e) {
            System.err.println("nash: " + NAME + ": invalid index format"); // Handle invalid index format
        } catch (Exception e) {
            System.err.println("nash: " + NAME + ": unexpected error"); // Handle unexpected errors
        }
        return null; // Return null if an error occurs
    }
}
