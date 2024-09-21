package shell.commands;

import shell.commands.shellCommands.Ptime;

import java.util.*;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.IOException;

public class Pipeline {
    public static void execute(LinkedList<String[]> commandStack, boolean isBackground) {
        if (isBackground) {
            // Run the command in a new thread (background)
            new Thread(() -> executeHelper(commandStack)).start();
        } else {
            // Run the command in the foreground
            executeHelper(commandStack);
        }
    }

    private static void executeHelper(LinkedList<String[]> commandStack) {
        long startTime = System.nanoTime();

        if (commandStack.isEmpty()) {
            return;
        }

        ProcessBuilder[] processBuilders = getProcessBuilders(commandStack);

        try {
            runProcesses(processBuilders);
        } catch (IOException ex) {
            System.err.println("nash: I/O error: " + ex.getMessage());
        } catch (InterruptedException ex) {
            Thread.currentThread().interrupt(); // Restore interrupted status
            System.err.println("nash: execution interrupted: " + ex.getMessage());
        }
        finally {
            long endTime = System.nanoTime();
            double elapsedTime = (endTime - startTime) / 1_000_000_000.0; // Convert to seconds
            Ptime.updateCumulativeTime(elapsedTime);
        }
    }

    private static void runProcesses(ProcessBuilder[] processBuilders) throws IOException, InterruptedException {
        Process[] processes = new Process[processBuilders.length];
        // Start the first process
        processes[0] = processBuilders[0].start();

        // Connect processes in a chain
        for (int i = 1; i < processBuilders.length; i++) {
            // Start the next process
            processes[i] = processBuilders[i].start();

            // Pipe output from the previous process to the next
            final int j = i; // Capture the index for use in the thread
            new Thread(() -> {
                try (InputStream in = processes[j - 1].getInputStream();
                     OutputStream out = processes[j].getOutputStream()) {

                    byte[] buffer = new byte[1024];
                    int bytesRead;
                    while ((bytesRead = in.read(buffer)) != -1) {
                        out.write(buffer, 0, bytesRead);
                    }
                    out.flush();
                } catch (IOException e) {
                    System.err.println("nash: pipe: error while piping data: " + e.getMessage());
                }
            }).start();
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
