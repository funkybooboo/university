package shell.commands.shellCommands;

public interface ShellCommand {
    String name = "command";
    void execute(String[] arguments);
}
