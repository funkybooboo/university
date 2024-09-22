package shell.commands;

import shell.commands.shellCommands.Ptime;

import java.util.*;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.IOException;

public class Pipeline {
    public static void execute(LinkedList<String[]> commandStack) {
        if (commandStack.isEmpty() || commandStack.size() < 2) {
            return;
        }

        ProcessBuilder[] processBuilders = getProcessBuilders(commandStack);

        long startTime = System.nanoTime();

        try {
            executeHelper(processBuilders);
        } catch (IOException ex) {
            System.err.println("nash: pipe: i/o exception: " + ex.getMessage());
        } catch (InterruptedException ex) {
            Thread.currentThread().interrupt(); // Restore interrupted status
            System.err.println("nash: pipe: execution interrupted: " + ex.getMessage());
        }
        finally {
            long endTime = System.nanoTime();
            double elapsedTime = (endTime - startTime) / 1_000_000_000.0; // Convert to seconds
            Ptime.updateCumulativeTime(elapsedTime);
        }
    }

    private static void executeHelper(ProcessBuilder[] processBuilders) throws IOException, InterruptedException {
        Process[] processes = new Process[processBuilders.length];

        // TODO make work with shell commands

        // Redirect the first process's input
        ProcessBuilder firstProcessBuilder = processBuilders[0];
        firstProcessBuilder.redirectInput(ProcessBuilder.Redirect.INHERIT);

        // Redirect the last process's output to the terminal
        ProcessBuilder lastProcessBuilder = processBuilders[processBuilders.length - 1];
        lastProcessBuilder.redirectOutput(ProcessBuilder.Redirect.INHERIT);

        // Start the first process
        processes[0] = firstProcessBuilder.start();

        // Connect processes in a chain
        for (int i = 1; i < processBuilders.length; i++) {
            processes[i] = processBuilders[i].start();

            // Pipe output from the previous process to the next
            try (InputStream in = processes[i - 1].getInputStream();
                 OutputStream out = processes[i].getOutputStream()) {
                byte[] buffer = new byte[1024];
                int bytesRead;
                while ((bytesRead = in.read(buffer)) != -1) {
                    out.write(buffer, 0, bytesRead);
                }
                out.flush();
            } catch (IOException e) {
                System.err.println("nash: pipe: error while piping data: " + e.getMessage());
            }
        }

        // Wait for all processes to complete
        for (Process process : processes) {
            process.waitFor();
        }
    }

    private static ProcessBuilder[] getProcessBuilders(LinkedList<String[]> commandStack) {
        ProcessBuilder[] processBuilders = new ProcessBuilder[commandStack.size()];

        // Prepare all process builders
        int index = 0;
        while (!commandStack.isEmpty()) {
            String[] commandParts = commandStack.poll();
            processBuilders[index++] = getProcessBuilder(commandParts);
        }
        return processBuilders;
    }

    private static ProcessBuilder getProcessBuilder(String[] commandParts) {
        String commandName = commandParts[0];
        String[] arguments = Arrays.copyOfRange(commandParts, 1, commandParts.length);

        List<String> commandList = new ArrayList<>();
        commandList.add(commandName);
        Collections.addAll(commandList, arguments);

        return new ProcessBuilder(commandList);
    }
}
