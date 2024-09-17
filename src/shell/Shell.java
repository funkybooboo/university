package shell;

import shell.commands.*;
import shell.commands.shellCommands.History;
import shell.commands.shellCommands.Ptime;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Shell {
    private final CommandFactory commandFactory = new CommandFactory();

    public void run() {
        Scanner scanner = new Scanner(System.in);
        while (true) { // the exit command will exit the program
            String currentDirectoryPath = System.getProperty("user.dir");
            System.out.print("[" + currentDirectoryPath + "]: ");
            String userInput = scanner.nextLine();
            execute(userInput);
        }
    }

    private void execute(String input) {
        boolean successfulExecution = true;
        long startTime = System.nanoTime();

        LinkedList<String> stack = getStack(input);

        String previousOutput = null;

        // TODO still need to implement &
        // TODO still need to fix pwd display

        while (!stack.isEmpty()) {
            String commandSegment = stack.poll();
            String[] commandParts = splitCommand(commandSegment);

            String commandName = commandParts[0];
            String[] commandArguments = Arrays.copyOfRange(commandParts, 1, commandParts.length);

            Command command = commandFactory.createCommand(commandName);
            Result result = command.execute(commandArguments, previousOutput);

            if (!result.isSuccess()) {
                System.out.println(result.getOutput());
                successfulExecution = false;
                break;
            }

            if (result.isCommand()) {
                stack.addAll(0, getStack(result.getOutput()));
            }

            if (stack.isEmpty()) {
                System.out.println(result.getOutput());
            }
            else {
                previousOutput = result.getOutput();
            }
        }

        long endTime = System.nanoTime();
        double elapsedTime = (endTime - startTime) / 1_000_000_000.0; // Convert to seconds
        Ptime.updateCumulativeTime(elapsedTime);
        if (successfulExecution) {
            History.addCommand(input);
        }
    }

    private static LinkedList<String> getStack(String input) {
        String[] commandSegments = input.split("\\|");
        LinkedList<String> stack = new LinkedList<>();
        for (String commandSegment : commandSegments) {
            stack.add(commandSegment.trim());
        }
        return stack;
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
