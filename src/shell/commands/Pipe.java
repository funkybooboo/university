package shell.commands;

import java.util.Arrays;
import java.util.LinkedList;

public class Pipe {
    public static void execute(LinkedList<String[]> commandStack) {

        String[] commandParts1 = commandStack.getFirst();
        String[] commandParts2 = commandStack.get(1);

        ProcessBuilder processBuilder1 = getProcessBuilder(commandParts1);
        ProcessBuilder processBuilder2 = getProcessBuilder(commandParts2);

        // Use the parent process's I/O channels
        processBuilder1.redirectInput(ProcessBuilder.Redirect.INHERIT);
        processBuilder2.redirectOutput(ProcessBuilder.Redirect.INHERIT);

        try {
            Process process1 = processBuilder1.start();
            Process process2 = processBuilder2.start();

            // this might feel backwards, but it is our program's input and output
            // thus process1's output is our input and our output is p2's input
            java.io.InputStream in = process1.getInputStream();
            java.io.OutputStream out = process2.getOutputStream();

            // Read the data from process1 and feed to p2.
            int data;
            while ((data = in.read()) != -1) {
                out.write(data);
            }

            // if we don't do this p2 won't know when we're done
            out.flush();
            out.close();

            process1.waitFor();
            process2.waitFor();
        }
        catch (Exception ex) {
            System.err.println("nash: unexpected error");
        }

    }

    private static ProcessBuilder getProcessBuilder(String[] commandParts) {
        String commandName = commandParts[0];
        String[] arguments = Arrays.copyOfRange(commandParts, 1, commandParts.length);
        if (arguments.length > 0) {
            return new ProcessBuilder(commandName, String.join(" ", arguments));
        }
        return new ProcessBuilder(commandName);
    }
}
