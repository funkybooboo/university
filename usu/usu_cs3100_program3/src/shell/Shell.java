package shell;

import shell.commands.shellCommands.History;

import java.util.LinkedList;

/**
 * The Shell class provides a simple command-line interface for executing user commands.
 *
 * @author Nate Stott
 */
public class Shell {
    private final CommandParser commandParser = new CommandParser();
    private final CommandExecutor commandExecutor = new CommandExecutor();
    private final SignalHandler signalHandler = new SignalHandler();
    private final Prompt prompt = new Prompt();

    /**
     * Starts the shell, sets up the signal handler, and enters the main command loop.
     * The loop continuously prompts the user for commands until the shell is exited.
     */
    public void run() {
        signalHandler.setup();

        System.out.println("Welcome to the shell: ");
        System.out.println("""
                                 _    \s
                                | |   \s
                 _ __   __ _ ___| |__ \s
                | '_ \\ / _` / __| '_ \\\s
                | | | | (_| \\__ \\ | | |
                |_| |_|\\__,_|___/_| |_|
                """);

        while (true) { // Control-C and exit will end this loop
            String userCommand = prompt.getUserCommand();

            if (userCommand.isEmpty()) {
                continue; // Skip empty commands
            }

            boolean isBackground = commandParser.isBackground(userCommand);
            userCommand = commandParser.filterUserCommand(userCommand, isBackground);

            LinkedList<String[]> commandStack;
            try {
                commandStack = commandParser.getCommandStack(userCommand);
            } catch (Exception ex) {
                commandExecutor.handleException(ex); // Handle parsing exceptions
                continue;
            }

            if (commandStack == null) {
                continue; // Skip if commandStack is null
            }

            // Store command in history after taking out the '^ [number]' commands
            History.addCommand(commandParser.getUserCommand(commandStack));

            // Execute the command and print output
            commandExecutor.execute(commandStack, isBackground);
        }
    }
}
