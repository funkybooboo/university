package shell.commands;


import shell.commands.shellCommands.Ptime;

import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class SystemCommand extends Command {
    public SystemCommand(String[] commandParts) {
        super(commandParts);
    }

    public String getName() {
        return commandParts[0];
    }

    public OutputStream execute(InputStream inputStream) throws Exception {
        List<String> commandList = new ArrayList<>();
        Collections.addAll(commandList, commandParts);

        ProcessBuilder processBuilder = new ProcessBuilder(commandList);
        processBuilder.directory(new File(System.getProperty("user.dir")));

        long startTime = System.nanoTime();

        try {
            Process process = processBuilder.start();

            // Handle process input
            try (OutputStream processOutput = process.getOutputStream()) {
                byte[] inputBytes = inputStream.readAllBytes();
                processOutput.write(inputBytes);
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

            process.waitFor();

            if (error.size() > 0) {
                throw new Exception(error.toString());
            }

            return output;
        } catch (IOException ex) {
            throw new Exception("nash: "+getName()+": invalid command", ex);
        } catch (Exception ex) {
            throw new Exception("nash: "+getName()+": unexpected error", ex);
        } finally {
            long endTime = System.nanoTime();
            double elapsedTime = (endTime - startTime) / 1_000_000_000.0; // Convert to seconds
            Ptime.updateCumulativeTime(elapsedTime);
        }
    }
}
