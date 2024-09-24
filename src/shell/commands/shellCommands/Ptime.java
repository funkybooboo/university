package shell.commands.shellCommands;

import shell.commands.Command;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

public class Ptime extends Command {
    public static final String NAME = "ptime";
    private static double cumulativeTime = 0.0;

    public Ptime(String[] commandParts) {
        super(commandParts);
    }

    public static void updateCumulativeTime(double time) {
        cumulativeTime += time;
    }

    @Override
    public OutputStream execute(InputStream inputStream, int commandIndex, int commandsLength) throws Exception {
        if (commandParts.length > 1) {
            throw new Exception("nash: "+NAME+": too many arguments\n");
        }
        String message = String.format("Total time in child processes: %.4f seconds%n", cumulativeTime);
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        output.write(message.getBytes());
        return output;
    }
}
