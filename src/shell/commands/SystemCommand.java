package shell.commands;

import shell.commands.shellCommands.Ptime;

import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The SystemCommand class represents a command that can be executed
 * in the system shell, allowing interaction with system processes.
 *
 * @author Nate Stott
 */
public class SystemCommand extends Command {

    /**
     * Constructs a SystemCommand with the specified command parts.
     *
     * @param commandParts The parts of the command to be executed.
     */
    public SystemCommand(String[] commandParts) {
        super(commandParts);
    }

    /**
     * Retrieves the name of the command.
     *
     * @return The command name.
     */
    public String getName() {
        return commandParts[0]; // Return the command name
    }

    /**
     * Executes the command using a ProcessBuilder.
     *
     * @param inputStream The input stream to provide input to the command.
     * @param commandIndex The index of the command in a pipeline.
     * @param commandsLength The total number of commands in the pipeline.
     * @return An output stream containing the command's output.
     * @throws Exception If an error occurs during execution.
     */
    public OutputStream execute(InputStream inputStream, int commandIndex, int commandsLength) throws Exception {
        List<String> commandList = new ArrayList<>();
        Collections.addAll(commandList, commandParts); // Prepare command list

        ProcessBuilder processBuilder = new ProcessBuilder(commandList);
        processBuilder.directory(new File(System.getProperty("user.dir"))); // Set working directory
        processBuilder.redirectError(ProcessBuilder.Redirect.INHERIT); // Redirect error output

        // Redirect input and output based on command position in pipeline
        if (commandIndex == 0) {
            processBuilder.redirectInput(ProcessBuilder.Redirect.INHERIT);
        }
        if (commandIndex == commandsLength - 1) {
            processBuilder.redirectOutput(ProcessBuilder.Redirect.INHERIT);
        }

        long startTime = System.nanoTime(); // Track execution time

        try {
            Process process = processBuilder.start(); // Start the process

            // Handle process input
            try (OutputStream processOutput = process.getOutputStream()) {
                byte[] inputBytes = inputStream.readAllBytes();
                processOutput.write(inputBytes); // Write input to process
                processOutput.flush();
            }

            // Capture process output
            ByteArrayOutputStream output = new ByteArrayOutputStream();
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    output.write(line.getBytes());
                    output.write(System.lineSeparator().getBytes()); // Maintain line breaks
                }
            }

            // Optionally read error output
            ByteArrayOutputStream error = new ByteArrayOutputStream();
            try (BufferedReader errorReader = new BufferedReader(new InputStreamReader(process.getErrorStream()))) {
                String line;
                while ((line = errorReader.readLine()) != null) {
                    error.write(line.getBytes());
                    error.write(System.lineSeparator().getBytes()); // Maintain line breaks
                }
            }

            process.waitFor(); // Wait for the process to complete

            if (error.size() > 0) {
                throw new Exception(error.toString()); // Handle errors from the process
            }

            return output; // Return the command output
        } catch (IOException ex) {
            throw new Exception("nash: " + getName() + ": invalid command", ex);
        } catch (Exception ex) {
            throw new Exception("nash: " + getName() + ": unexpected error", ex);
        } finally {
            long endTime = System.nanoTime();
            double elapsedTime = (endTime - startTime) / 1_000_000_000.0; // Convert to seconds
            Ptime.updateCumulativeTime(elapsedTime); // Update execution time
        }
    }
}
