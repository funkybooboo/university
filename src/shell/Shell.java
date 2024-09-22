package shell;

import shell.commands.Command;
import shell.commands.shellCommands.Past;
import shell.commands.shellCommands.History;
import shell.commands.CommandFactory;
import shell.commands.Pipeline;
import sun.misc.Signal;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Shell {
    private final CommandFactory commandFactory = new CommandFactory();

    public void run() {
        // Handle control-c
        Signal.handle(new Signal("INT"), sig -> System.exit(1));

        try (Scanner scanner = new Scanner(System.in)) {
            while (true) { // exit command will end the program
                System.out.print("[" + System.getProperty("user.dir") + "]: ");
                String userCommand = scanner.nextLine().trim();
                if (userCommand.isEmpty()) {
                    continue;
                }
                if (isInvalid(userCommand)) {
                    System.err.println("nash: error: invalid use of &");
                    continue;
                }
                execute(userCommand);
            }
        }
    }

    private void execute(String userCommand) {
        boolean isBackground = isBackground(userCommand);
        if (isBackground) {
            userCommand = userCommand.replaceAll("^\\s*\\(\\s*|\\s*\\)\\s*|\\s*&\\s*$", "").trim();
        }

        LinkedList<String[]> commandStack;
        try {
            commandStack = getCommandStack(userCommand);
            if (commandStack == null) {
                return;
            }

            History.addCommand(getUserCommand(commandStack));

            if (isBackground) {
                new Thread(() -> executeAllCommands(commandStack)).start();
            } else {
                executeAllCommands(commandStack);
            }
        } catch (Exception ex) {
            System.err.println(Optional
                    .ofNullable(ex.getMessage())
                    .orElse("")
            );
        }
    }

    private void executeAllCommands(LinkedList<String[]> commandStack) {
        if (commandStack.size() == 1) {
            executeCommand(commandStack.poll());
        } else {
            Pipeline.execute(commandStack);
        }
    }

    private void executeCommand(String[] commandParts) {
        Command command = commandFactory.createCommand(commandParts[0]);
        command.execute(Arrays.copyOfRange(commandParts, 1, commandParts.length));
    }

    private boolean isInvalid(String userCommand) {
        return Pattern
                .compile("(^|\\s)(&|\\|\\s*&|&\\s*\\|)(\\s|$)(?!\\s*$)")
                .matcher(userCommand)
                .find();
    }

    private boolean isBackground(String userCommand) {
        return Pattern
                .compile("\\s*\\((\\s*[^()]*?(\\s*\\|\\s*[^()]*?)+\\s*)\\)\\s*&\\s*|\\s*[^()]+\\s*&\\s*")
                .matcher(userCommand)
                .find();
    }

    private String getUserCommand(LinkedList<String[]> commandStack) {
        StringBuilder input = new StringBuilder();
        for (int i = 0; i < commandStack.size(); i++) {
            input.append(String.join(" ", commandStack.get(i)));
            if (i < commandStack.size() - 1) {
                input.append(" | ");
            }
        }
        return input.toString();
    }

    private LinkedList<String[]> getCommandStack(String userCommand) throws Exception {
        if (userCommand == null) return null;
        LinkedList<String[]> commandStack = new LinkedList<>();
        for (String commandSegment : userCommand.split("\\|")) {
            String segment = commandSegment.trim();
            if (segment.isEmpty()) throw new Exception("nash: expected a command, but found a pipe");
            String[] commandParts = splitCommand(segment);
            if (Objects.equals(commandParts[0], Past.name)) {
                String pastUserCommand = Past.execute(Arrays.copyOfRange(commandParts, 1, commandParts.length));
                if (pastUserCommand == null) throw new Exception();
                commandStack.addAll(getCommandStack(pastUserCommand));
            } else {
                commandStack.add(commandParts);
            }
        }
        return commandStack;
    }

    private String[] splitCommand(String command) {
        List<String> matchList = new ArrayList<>();
        Matcher regexMatcher = Pattern.compile("[^\\s\"']+|\"([^\"]*)\"|'([^']*)'").matcher(command);
        while (regexMatcher.find()) {
            matchList.add(Optional
                    .ofNullable(regexMatcher.group(1))
                    .orElse(Optional.ofNullable(regexMatcher.group(2))
                    .orElse(regexMatcher.group()))
            );
        }
        return matchList.toArray(new String[0]);
    }
}
