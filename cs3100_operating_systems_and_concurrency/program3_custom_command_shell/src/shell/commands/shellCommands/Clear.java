package shell.commands.shellCommands;

import shell.commands.Command;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * The Clear class represents a command to clear the terminal screen.
 * It extends the Command class and implements the execute method.
 *
 * @author Nate Stott
 */
public class Clear extends Command {
    public static final String NAME = "clear"; // Command name for clearing the screen

    /**
     * Constructs a Clear command with the specified command parts.
     *
     * @param commandParts The parts of the command.
     */
    public Clear(String[] commandParts) {
        super(commandParts);
    }

    /**
     * Executes the Clear command to clear the terminal screen.
     *
     * @param inputStream The input stream (not used in this command).
     * @param commandIndex The index of the command in a pipeline.
     * @param commandsLength The total number of commands in the pipeline.
     * @return An empty output stream (not used).
     * @throws Exception If the command arguments are invalid or if clearing the screen fails.
     */
    @Override
    public OutputStream execute(InputStream inputStream, int commandIndex, int commandsLength) throws Exception {
        if (commandParts.length > 1) {
            throw new Exception("nash: " + NAME + ": invalid number of arguments"); // Validate number of arguments
        }

        String os = System.getProperty("os.name").toLowerCase();
        try {
            if (os.contains("win")) {
                // Clear the screen on Windows
                new ProcessBuilder("cmd", "/c", "cls").inheritIO().start().waitFor();
            } else {
                // Clear the screen on Unix-based systems
                System.out.print("\033[H\033[2J"); // Clear the screen
                System.out.flush(); // Flush output to the terminal
                System.out.print("\033[1;1H"); // Move cursor to the top left
            }
        } catch (Exception e) {
            throw new Exception("nash: " + NAME + ": error clearing screen: " + e.getMessage()); // Handle errors
        }
        return new ByteArrayOutputStream(); // Return empty stream (not used)
    }
}
