package shell;

import shell.commands.shellCommands.Past;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The CommandParser class is responsible for parsing user commands,
 * handling command pipelines, and checking for background execution.
 *
 * @author Nate Stott
 */
public class CommandParser {

    /**
     * Parses the user command into a stack of commands.
     *
     * @param userCommand The command entered by the user.
     * @return A linked list of parsed command segments.
     * @throws Exception If the command syntax is invalid.
     */
    public LinkedList<String[]> getCommandStack(String userCommand) throws Exception {
        if (userCommand == null) {
            return null; // Return null if userCommand is null
        }

        LinkedList<String[]> commandStack = new LinkedList<>();
        for (String commandSegment : userCommand.split("\\|")) {
            String segment = commandSegment.trim();
            if (segment.isEmpty()) {
                throw new Exception("nash: expected a command, but found a pipe"); // Handle empty command segment
            }

            String[] commandParts = splitCommand(segment);
            if (Objects.equals(commandParts[0], Past.NAME)) {
                String pastUserCommand = Past.execute(Arrays.copyOfRange(commandParts, 1, commandParts.length));
                if (pastUserCommand == null) {
                    throw new Exception(); // Handle case where past command is null
                }
                commandStack.addAll(getCommandStack(pastUserCommand)); // Recursively parse past command
            } else {
                commandStack.add(commandParts); // Add the current command parts to the stack
            }
        }
        return commandStack;
    }

    /**
     * Splits a command string into its constituent parts while preserving quotes.
     *
     * @param command The command string to split.
     * @return An array of command parts.
     */
    private String[] splitCommand(String command) {
        List<String> matchList = new ArrayList<>();
        Matcher regexMatcher = Pattern.compile("\"([^\"]*)\"|'([^']*)'|`([^`]*)`|([^\\s\"'`]+)").matcher(command);

        while (regexMatcher.find()) {
            // Add the whole match, preserving quotes
            if (regexMatcher.group(1) != null) {
                matchList.add("\"" + regexMatcher.group(1) + "\"");
            } else if (regexMatcher.group(2) != null) {
                matchList.add("'" + regexMatcher.group(2) + "'");
            } else if (regexMatcher.group(3) != null) {
                matchList.add("`" + regexMatcher.group(3) + "`");
            } else if (regexMatcher.group(4) != null) {
                matchList.add(regexMatcher.group(4));
            }
        }

        return matchList.toArray(new String[0]); // Return the list as an array
    }


    /**
     * Checks if a command should be run in the background.
     *
     * @param userCommand The command entered by the user.
     * @return true if the command should run in the background, false otherwise.
     */
    public boolean isBackground(String userCommand) {
        return Pattern
                .compile("\\s*\\((\\s*[^()]*?(\\s*\\|\\s*[^()]*?)+\\s*)\\)\\s*&\\s*|\\s*[^()]+\\s*&\\s*")
                .matcher(userCommand)
                .find(); // Check for background execution pattern
    }

    /**
     * Filters the user command to remove background execution indicators.
     *
     * @param userCommand The command entered by the user.
     * @param isBackground Indicates if the command is meant to run in the background.
     * @return The filtered command string.
     */
    public String filterUserCommand(String userCommand, boolean isBackground) {
        if (isBackground) {
            userCommand = userCommand.replaceAll("^\\s*\\(\\s*|\\s*\\)\\s*|\\s*&\\s*$", "").trim(); // Clean background command
        }
        return userCommand;
    }

    /**
     * Constructs a user command string from a stack of command segments.
     *
     * @param commandStack The stack of parsed command segments.
     * @return The reconstructed command string.
     */
    public String getUserCommand(LinkedList<String[]> commandStack) {
        StringBuilder input = new StringBuilder();
        for (int i = 0; i < commandStack.size(); i++) {
            input.append(String.join(" ", commandStack.get(i))); // Join command parts
            if (i < commandStack.size() - 1) {
                input.append(" | "); // Add pipeline separator
            }
        }
        return input.toString(); // Return the constructed command string
    }
}
