package shell.commands;

import java.util.ArrayList;
import java.util.List;

public class History implements Command {
    static String name = "history";
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
    public boolean execute(String[] arguments) {
        if (arguments.length == 0) {
            for (int i = 0; i < commandHistory.size(); i++) {
                System.out.println((i + 1) + ": " + commandHistory.get(i));
            }
        }
        else {
            System.out.println("nash: history: too many args for history command");
            return false;
        }
        return true;
    }
}
