package shell;

import shell.commands.shellCommands.Past;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CommandParser {

    public LinkedList<String[]> getCommandStack(String userCommand) throws Exception {
        if (userCommand == null) return null;
        LinkedList<String[]> commandStack = new LinkedList<>();
        for (String commandSegment : userCommand.split("\\|")) {
            String segment = commandSegment.trim();
            if (segment.isEmpty()) throw new Exception("nash: expected a command, but found a pipe");
            String[] commandParts = splitCommand(segment);
            if (Objects.equals(commandParts[0], Past.NAME)) {
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

    public boolean isBackground(String userCommand) {
        return Pattern
                .compile("\\s*\\((\\s*[^()]*?(\\s*\\|\\s*[^()]*?)+\\s*)\\)\\s*&\\s*|\\s*[^()]+\\s*&\\s*")
                .matcher(userCommand)
                .find();
    }

    public String filterUserCommand(String userCommand, boolean isBackground) {
        if (isBackground) {
            userCommand = userCommand.replaceAll("^\\s*\\(\\s*|\\s*\\)\\s*|\\s*&\\s*$", "").trim();
        }
        return userCommand;
    }

    public String getUserCommand(LinkedList<String[]> commandStack) {
        StringBuilder input = new StringBuilder();
        for (int i = 0; i < commandStack.size(); i++) {
            input.append(String.join(" ", commandStack.get(i)));
            if (i < commandStack.size() - 1) {
                input.append(" | ");
            }
        }
        return input.toString();
    }
}
