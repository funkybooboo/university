package shell.commands;

public interface Command {
    String name = "command";
    void execute(String[] arguments);
}
