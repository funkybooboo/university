package shell;

import shell.commands.shellCommands.History;

import java.util.LinkedList;

public class Shell {
    private final CommandParser commandParser = new CommandParser();
    private final CommandExecutor commandExecutor = new CommandExecutor();
    private final SignalHandler signalHandler = new SignalHandler();
    private final Prompt prompt = new Prompt();

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

        while (true) { // control-c and exit will end this loop
            String userCommand;
            try {
                userCommand = prompt.getUserCommand();
            }
            catch (Exception ex) {
                commandExecutor.handleException(ex);
                continue;
            }

            if (userCommand.isEmpty()) {
                continue;
            }

            boolean isBackground = commandParser.isBackground(userCommand);
            userCommand = commandParser.filterUserCommand(userCommand, isBackground);

            LinkedList<String[]> commandStack;
            try {
                commandStack = commandParser.getCommandStack(userCommand);
            }
            catch (Exception ex) {
                commandExecutor.handleException(ex);
                continue;
            }

            if (commandStack == null) {
                continue;
            }

            // Store command in history
            History.addCommand(userCommand);

            // Execute the command and print output
            commandExecutor.execute(commandStack, isBackground);
        }
    }
}
