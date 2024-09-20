package shell.commands;

import shell.commands.shellCommands.Ptime;

import java.io.File;
import java.io.IOException;

public class SystemCommand implements Command {
    private final String commandName;

    public SystemCommand(String commandName) {
        this.commandName = commandName;
    }

    @Override
    public void execute(String[] arguments) {
        long startTime = System.nanoTime();

        ProcessBuilder processBuilder;
        if (arguments.length > 0) {
            processBuilder = new ProcessBuilder(commandName, String.join(" ", arguments));
        } else {
            processBuilder = new ProcessBuilder(commandName);
        }

        processBuilder.directory(new File(System.getProperty("user.dir")));

        processBuilder.redirectInput(ProcessBuilder.Redirect.INHERIT);
        processBuilder.redirectOutput(ProcessBuilder.Redirect.INHERIT);

        try {
            Process process = processBuilder.start();
            process.waitFor();
        }
        catch (IOException ex) {
            System.err.println("nash: "+commandName+": invalid command");
        }
        catch (Exception ex) {
            System.err.println("nash: "+commandName+": unexpected error");
        }
        finally {
            long endTime = System.nanoTime();
            double elapsedTime = (endTime - startTime) / 1_000_000_000.0; // Convert to seconds
            Ptime.updateCumulativeTime(elapsedTime);
        }
    }
}
