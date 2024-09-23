package shell.commands.shellCommands;

import shell.commands.Command;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

public class History extends Command {
    public static final String NAME = "history";
    private static final List<String> commandHistory = new ArrayList<>();

    public History(String[] commandParts) {
        super(commandParts);
    }

    public static String getCommand(int index) {
        return commandHistory.get(index);
    }

    public static void addCommand(String command) {
        commandHistory.add(command);
    }

    // The shell should keep a history of the previous shell.commands entered, with no maximum hard-coded history length.
    // If the user enters the command 'history', the shell should provide a listing of the complete command shell history.
    @Override
    public OutputStream execute(InputStream inputStream) throws Exception {
        if (commandParts.length > 1) {
            throw new Exception("nash: "+NAME+": too many args for history command");
        }
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        output.write("-- Command History --\n".getBytes());
        for (int i = 0; i < commandHistory.size(); i++) {
            output.write(((i + 1)+" : "+commandHistory.get(i)+"\n").getBytes());
        }
        return output;
    }
}
