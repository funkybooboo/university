package shell.commands.shellCommands;

public class Exit implements ShellCommand {
    public static String name = "exit";
    // The command 'exit' is used to terminate your shell program, when entered, your shell program should end.
    @Override
    public void execute(String[] arguments) {
        if (arguments.length == 0) {
            System.exit(0);
        }
        if (arguments.length == 1) {
            try {
                System.exit(Integer.parseInt(arguments[0]));
            }
            catch (Exception e) {
                System.err.println("nash: exit: invalid exit code");
            }
        }
        else {
            System.err.println("nash: exit: too many arguments");
        }
    }
}
