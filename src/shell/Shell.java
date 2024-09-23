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
