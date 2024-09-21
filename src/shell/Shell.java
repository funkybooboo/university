package shell;

import shell.commands.Past;
import shell.commands.shellCommands.History;
import shell.commands.Command;
import shell.commands.CommandFactory;
import shell.commands.Pipeline;


import java.util.*;
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
        if (checkMiddle(input)) return;

        LinkedList<String[]> commandStack;
        try {
            commandStack = getCommandStack(input);
        }
        catch (Exception ex) {
            String m = ex.getMessage();
            if (m == null || m.isEmpty()) {
                return;
            }
            System.err.println(m);
            return;
        }

        History.addCommand(getInput(commandStack));

        // TODO fix the path displayed on the prompt (cd)

        boolean isBackground = checkBackground(input);

        if (commandStack.size() == 1) {
            runCommand(commandStack.poll(), isBackground);
        }
        else if (commandStack.size() > 1) {
            Pipeline.execute(commandStack, isBackground);
        }
    }

    private void runCommand(String[] commandParts, boolean isBackground) {
        String commandName = commandParts[0];
        String[] commandArguments = Arrays.copyOfRange(commandParts, 1, commandParts.length);

        Command command = commandFactory.createCommand(commandName);

        if (isBackground) {
            // Run the command in a new thread (background)
            new Thread(() -> command.execute(commandArguments)).start();
        } else {
            // Run the command in the foreground
            command.execute(commandArguments);
        }
    }

    private boolean checkMiddle(String input) {
        String regex = "\\|\\s*.*?&\\s*.*?\\|";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(input);
        boolean isMiddle = matcher.find();
        if (isMiddle) {
            System.err.println("nash: error background command in the middle of a pipeline");
            return true;
        }
        return false;
    }

    private boolean checkBackground(String input) {
        String regex = "\\s*\\((\\s*[^()]*?(\\s*\\|\\s*[^()]*?)+\\s*)\\)\\s*&\\s*|\\s*[^()]+\\s*&\\s*";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(input);
        return matcher.find();
    }

    private String getInput(LinkedList<String[]> commandStack) {
        StringBuilder input = new StringBuilder();
        for (int i = 0; i < commandStack.size(); i++) {
            String[] commandParts = commandStack.get(i);
            input.append(String.join(" ", commandParts));
            if (i < commandStack.size()-1) {
                input.append(" | ");
            }
        }
        return input.toString();
    }

    private LinkedList<String[]> getCommandStack(String input) throws Exception {
        String[] commandSegments = input.split("\\|");
        LinkedList<String[]> commandStack = new LinkedList<>();
        for (String commandSegment : commandSegments) {
            if (commandSegment.trim().isEmpty()) {
                throw new Exception("nash: expected a command, but found a pipe");
            }
            String[] commandParts = splitCommand(commandSegment.trim());
            String commandName = commandParts[0];
            String[] commandArguments = Arrays.copyOfRange(commandParts, 1, commandParts.length);
            if (Objects.equals(commandName, Past.name)) {
                String otherInput = Past.execute(commandArguments);
                if (otherInput == null) {
                    throw new Exception();
                }
                commandStack.addAll(getCommandStack(otherInput));
            }
            else {
                commandStack.add(commandParts);
            }
        }
        return commandStack;
    }

    /**
     * Split the user command by spaces, but preserving them when inside double-quotes.
     * Code Adapted from: <a href="https://stackoverflow.com/questions/366202/regex-for-splitting-a-string-using-space-when-not-surrounded-by-single-or-double">link</a>
     */
    private String[] splitCommand(String command) {
        List<String> matchList = new ArrayList<>();

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
