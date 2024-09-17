package shell.commands;

public interface Command {
    String name = "command";
    Result execute(String[] arguments, String previousOutput);
}
