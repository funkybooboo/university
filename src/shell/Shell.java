package shell;

import shell.commands.shellCommands.History;

import java.util.LinkedList;
import java.util.Scanner;

public class Shell {
    private final CommandParser commandParser = new CommandParser();
    private final CommandExecutor commandExecutor = new CommandExecutor();
    private final SignalHandler signalHandler = new SignalHandler();

    public void run() {
        signalHandler.setup();

        System.out.println("Welcome to the custom shell: ");
        System.out.println("""
                                 _    \s
                                | |   \s
                 _ __   __ _ ___| |__ \s
                | '_ \\ / _` / __| '_ \\\s
                | | | | (_| \\__ \\ | | |
                |_| |_|\\__,_|___/_| |_|
                                      \s
                                      \s
                """);

        try (Scanner scanner = new Scanner(System.in)) {
            while (true) {
                System.out.print("[" + System.getProperty("user.dir") + "]: ");
                String userCommand = scanner.nextLine().trim();
                if (userCommand.isEmpty()) continue;

                boolean isBackground = commandParser.isBackground(userCommand);
                userCommand = commandParser.filterUserCommand(userCommand, isBackground);

                LinkedList<String[]> commandStack = null;
                try {
                    commandStack = commandParser.getCommandStack(userCommand);
                } catch (Exception ex) {
                    commandExecutor.handleException(ex);
                }

                if (commandStack == null) return;

                History.addCommand(commandParser.getUserCommand(commandStack));

                commandExecutor.execute(commandStack, isBackground);
            }
        }
    }
}
