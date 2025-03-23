package shell.commands;

import java.io.*;
import java.util.LinkedList;

/**
 * The Pipeline class manages the execution of a sequence of commands,
 * facilitating communication between them via input and output streams.
 *
 * @author Nate Stott
 */
public class Pipeline {
    private final CommandFactory commandFactory = new CommandFactory();

    /**
     * Executes a stack of commands in a pipeline.
     *
     * @param commandStack The stack of commands to execute.
     * @return An output stream containing the final command's output.
     * @throws Exception If an error occurs during execution.
     */
    public OutputStream execute(LinkedList<String[]> commandStack) throws Exception {
        if (commandStack.isEmpty()) {
            return new ByteArrayOutputStream(); // Return empty output if no commands
        }

        Command[] commands = getCommands(commandStack); // Retrieve command objects
        OutputStream[] outputStreams = new OutputStream[commands.length]; // Array for output streams

        // Execute the first command with empty input
        Command firstCommand = commands[0];
        outputStreams[0] = firstCommand.execute(new ByteArrayInputStream(new byte[0]), 0, commands.length);

        // Execute subsequent commands, piping output to input
        for (int i = 1; i < commands.length; i++) {
            outputStreams[i] = commands[i].execute(outputStreamToInputStream(outputStreams[i - 1]), i, commands.length);
        }

        return outputStreams[outputStreams.length - 1]; // Return the final output stream
    }

    /**
     * Converts an OutputStream to an InputStream.
     *
     * @param outputStream The OutputStream to convert.
     * @return An InputStream representing the output stream's data.
     * @throws IOException If the conversion fails.
     */
    private InputStream outputStreamToInputStream(OutputStream outputStream) throws IOException {
        if (outputStream instanceof ByteArrayOutputStream byteArrayOutputStream) {
            return new ByteArrayInputStream(byteArrayOutputStream.toByteArray()); // Convert to InputStream
        } else {
            throw new IOException("nash: need to implement OutputStream subtype conversion");
        }
    }

    /**
     * Retrieves an array of Command objects from the command stack.
     *
     * @param commandStack The stack of command segments.
     * @return An array of Command objects.
     */
    private Command[] getCommands(LinkedList<String[]> commandStack) {
        Command[] commands = new Command[commandStack.size()];
        int index = 0;

        while (!commandStack.isEmpty()) {
            commands[index++] = commandFactory.createCommand(commandStack.poll()); // Create command objects
        }
        return commands; // Return the array of commands
    }
}
