package shell.commands.shellCommands;

import shell.commands.Command;
import shell.commands.Result;

import java.util.ArrayList;
import java.util.List;

public class History implements Command {
    public static String name = "history";
    private static final List<String> commandHistory = new ArrayList<>();

    public static String getCommand(int index) {
        return commandHistory.get(index);
    }

    public static void addCommand(String command) {
        commandHistory.add(command);
    }

    // The shell should keep a history of the previous shell.commands entered, with no maximum hard-coded history length.
    // If the user enters the command 'history', the shell should provide a listing of the complete command shell history.
    @Override
    public Result execute(String[] arguments, String previousOutput) {
        if (arguments.length > 0) {
            return new Result("nash: history: too many args for history command", false);
        }
        StringBuilder stringBuilder = new StringBuilder();
        for (int i = 0; i < commandHistory.size(); i++) {
            stringBuilder.append((i + 1)).append(" : ").append(commandHistory.get(i)).append("\n");
        }
        return new Result(stringBuilder.toString(), true);
    }
}
