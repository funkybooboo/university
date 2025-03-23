package shell;

import java.util.Scanner;

/**
 * The Prompt class handles user input for the shell application.
 * It displays the current directory as part of the prompt.
 *
 * @author Nate Stott
 */
public class Prompt {
    private final Scanner scanner = new Scanner(System.in);

    /**
     * Retrieves the user's command input.
     *
     * @return The command entered by the user.
     */
    public String getUserCommand() {
        String currentDirPath = System.getProperty("user.dir");
        String prompt = String.format("[%s]: ", currentDirPath); // Format prompt with current directory
        System.out.print(prompt);
        return scanner.nextLine(); // Get user input
    }
}
