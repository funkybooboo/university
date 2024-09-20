package shell;

import shell.commands.*;
import shell.commands.shellCommands.History;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Shell {
    private final CommandFactory commandFactory = new CommandFactory();

    public void run() {
        Scanner scanner = new Scanner(System.in);
        while (true) { // the exit command will end the program
            String currentDirectoryPath = System.getProperty("user.dir");
            System.out.print("[" + currentDirectoryPath + "]: ");
            String userInput = scanner.nextLine();
            execute(userInput);
        }
    }

    private void execute(String input) {
        History.addCommand(input);

        LinkedList<String[]> commandStack = getCommandStack(input);

        // TODO implement & (dont wait for the process to finish)
        //  question: what happens if used in |
        // TODO fix the path displayed on the prompt (cd)
        // TODO fix system processes (use child processes)
        // TODO fix pipe (use child processes)

        if (commandStack.size() == 1) {
            String[] commandParts = commandStack.poll();

            String commandName = commandParts[0];
            String[] commandArguments = Arrays.copyOfRange(commandParts, 1, commandParts.length);

            Command command = commandFactory.createCommand(commandName);
            command.execute(commandArguments);
        }
        if (commandStack.size() > 1) {
            PipeCommand pipeCommand = new PipeCommand(commandStack);

        }
    }

    private LinkedList<String[]> getCommandStack(String input) {
        String[] commandSegments = input.split("\\|");
        LinkedList<String[]> commandStack = new LinkedList<>();
        for (String commandSegment : commandSegments) {
            String[] commandParts = splitCommand(commandSegment.trim());
            commandStack.add(commandParts);
        }
        return commandStack;
    }

    /**
     * Split the user command by spaces, but preserving them when inside double-quotes.
     * Code Adapted from: <a href="https://stackoverflow.com/questions/366202/regex-for-splitting-a-string-using-space-when-not-surrounded-by-single-or-double">link</a>
     */
    private String[] splitCommand(String command) {
        java.util.List<String> matchList = new java.util.ArrayList<>();

        Pattern regex = Pattern.compile("[^\\s\"']+|\"([^\"]*)\"|'([^']*)'");
        Matcher regexMatcher = regex.matcher(command);
        while (regexMatcher.find()) {
            if (regexMatcher.group(1) != null) {
                // Add double-quoted string without the quotes
                matchList.add(regexMatcher.group(1));
            } else if (regexMatcher.group(2) != null) {
                // Add single-quoted string without the quotes
                matchList.add(regexMatcher.group(2));
            } else {
                // Add unquoted word
                matchList.add(regexMatcher.group());
            }
        }

        return matchList.toArray(new String[0]);
    }
}
