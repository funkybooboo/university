package shell;

import shell.commands.*;

import java.util.Arrays;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Shell {
    private static final CommandFactory commandFactory = new CommandFactory();

    public static void run() {
        Scanner scanner = new Scanner(System.in);
        while (true) { // the exit command will exit the program
            String currentDirectoryPath = System.getProperty("user.dir");
            System.out.print("[" + currentDirectoryPath + "]: ");
            String userInput = scanner.nextLine();
            Shell.execute(userInput);
        }
    }

    public static void execute(String input) {

        String[] commandSegments = input.split("\\|");
        boolean successfulExecution = true;
        long startTime = System.nanoTime();

        for (String commandSegment : commandSegments) {
            String[] commandParts = splitCommand(commandSegment);

            String commandName = commandParts[0];
            String[] commandArguments = Arrays.copyOfRange(commandParts, 1, commandParts.length);

            Command command = commandFactory.createCommand(commandName);
            Result result = command.execute(commandArguments);

            if (!result.isSuccess()) {
                System.out.println(result.getOutput());
                successfulExecution = false;
                break;
            }

            // TODO how to feed output to the next command as input

        }

        long endTime = System.nanoTime();
        double elapsedTime = (endTime - startTime) / 1_000_000_000.0; // Convert to seconds
        Ptime.updateCumulativeTime(elapsedTime);
        if (successfulExecution) {
            History.addCommand(input);
        }
    }

    /**
     * Split the user command by spaces, but preserving them when inside double-quotes.
     * Code Adapted from: <a href="https://stackoverflow.com/questions/366202/regex-for-splitting-a-string-using-space-when-not-surrounded-by-single-or-double">link</a>
     */
    private static String[] splitCommand(String command) {
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
