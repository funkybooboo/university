package shell;

import shell.commands.Pipeline;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.LinkedList;
import java.util.Optional;

/**
 * The CommandExecutor class is responsible for executing parsed commands,
 * either in the foreground or background.
 *
 * @author Nate Stott
 */
public class CommandExecutor {

    private final Pipeline pipeline = new Pipeline();

    /**
     * Executes the command stack based on whether it should run in the background or foreground.
     *
     * @param commandStack The stack of parsed commands to execute.
     * @param isBackground Indicates if the command should run in the background.
     */
    public void execute(LinkedList<String[]> commandStack, boolean isBackground) {
        if (isBackground) {
            executeInBackground(commandStack); // Run command in background
        } else {
            executeInForeground(commandStack); // Run command in foreground
        }
    }

    /**
     * Executes the command stack in the foreground.
     *
     * @param commandStack The stack of parsed commands to execute.
     */
    public void executeInForeground(LinkedList<String[]> commandStack) {
        execute(commandStack); // Directly execute in foreground
    }

    /**
     * Executes the command stack in the background.
     *
     * @param commandStack The stack of parsed commands to execute.
     */
    private void executeInBackground(LinkedList<String[]> commandStack) {
        new Thread(() -> execute(commandStack)).start(); // Start a new thread for background execution
    }

    /**
     * Executes the command stack and handles output.
     *
     * @param commandStack The stack of parsed commands to execute.
     */
    private void execute(LinkedList<String[]> commandStack) {
        try {
            printOutputStream(pipeline.execute(commandStack)); // Execute and print output
        } catch (Exception ex) {
            handleException(ex); // Handle exceptions during execution
        }
    }

    /**
     * Handles exceptions by printing error messages to stderr.
     *
     * @param ex The exception to handle.
     */
    public void handleException(Exception ex) {
        System.err.println(Optional.ofNullable(ex.getMessage()).orElse("")); // Print error message
    }

    /**
     * Prints the output from the executed command.
     *
     * @param outputStream The output stream containing the command output.
     * @throws IOException If an I/O error occurs.
     */
    private void printOutputStream(OutputStream outputStream) throws IOException {
        String output;
        if (outputStream instanceof ByteArrayOutputStream byteArrayOutputStream) {
            output = byteArrayOutputStream.toString(); // Convert output stream to string
        } else {
            throw new IOException("nash: need to implement OutputStream subtype output");
        }
        System.out.print(output); // Print the output
    }
}
