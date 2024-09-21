package shell.commands;


import shell.commands.shellCommands.Ptime;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class SystemCommand {
    public static void execute(String commandName, String[] arguments, boolean isBackground) {
        long startTime = System.nanoTime();

        List<String> commandList = new ArrayList<>();
        commandList.add(commandName);
        Collections.addAll(commandList, arguments);

        ProcessBuilder processBuilder = new ProcessBuilder(commandList);

        processBuilder.directory(new File(System.getProperty("user.dir")));

        processBuilder.redirectInput(ProcessBuilder.Redirect.INHERIT);
        processBuilder.redirectOutput(ProcessBuilder.Redirect.INHERIT);

        try {
            Process process = processBuilder.start();
            if (!isBackground) {
                process.waitFor();
            }
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
