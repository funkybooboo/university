package shell.commands;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;

public class SystemCommand implements Command {
    private final String commandName;

    public SystemCommand(String commandName) {
        this.commandName = commandName;
    }

    @Override
    public Result execute(String[] arguments, String previousOutput) {
        ProcessBuilder processBuilder;
        if (arguments.length > 0) {
            processBuilder = new ProcessBuilder(commandName, String.join(" ", arguments));
        } else {
            processBuilder = new ProcessBuilder(commandName);
        }

        StringBuilder output = new StringBuilder();
        StringBuilder errorOutput = new StringBuilder();

        try {
            Process process = processBuilder.start();

            if (previousOutput != null) {
                try (OutputStream out = process.getOutputStream()) {
                    out.write(previousOutput.getBytes(StandardCharsets.UTF_8));
                }
            }

            // Read standard output
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    output.append(line).append("\n");
                }
            }

            // Read standard error
            try (BufferedReader errorReader = new BufferedReader(new InputStreamReader(process.getErrorStream()))) {
                String line;
                while ((line = errorReader.readLine()) != null) {
                    errorOutput.append(line).append("\n");
                }
            }

            int exitCode = process.waitFor();
            if (exitCode != 0) {
                String errorMessage = !errorOutput.isEmpty() ? errorOutput.toString() : "command failed with exit code " + exitCode;
                return new Result(errorMessage, false);
            }

        } catch (IOException e) {
            return new Result("I/O error occurred: " + e.getMessage(), false);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt(); // Restore interrupted status
            return new Result("Process was interrupted: " + e.getMessage(), false);
        }

        return new Result(output.toString(), true);
    }

}
