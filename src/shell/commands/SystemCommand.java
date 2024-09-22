package shell.commands;


import shell.commands.shellCommands.Ptime;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class SystemCommand implements Command {
    private final String commandName;

    public SystemCommand(String commandName) {
        this.commandName = commandName;
    }

    public void execute(String[] arguments) {
        List<String> commandList = new ArrayList<>();
        commandList.add(commandName);
        Collections.addAll(commandList, arguments);

        ProcessBuilder processBuilder = new ProcessBuilder(commandList);

        processBuilder.directory(new File(System.getProperty("user.dir")));

        processBuilder.redirectInput(ProcessBuilder.Redirect.INHERIT);
        processBuilder.redirectOutput(ProcessBuilder.Redirect.INHERIT);
        processBuilder.redirectError(ProcessBuilder.Redirect.INHERIT);

        long startTime = System.nanoTime();

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
