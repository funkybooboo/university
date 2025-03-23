package shell.commands;

import shell.commands.shellCommands.*;

import java.util.HashMap;
import java.util.Map;

/**
 * The CommandFactory class is responsible for creating command instances
 * based on the command name provided. It maps command names to their respective
 * command classes.
 *
 * @author Nate Stott
 */
public class CommandFactory {

    private final Map<String, Class<? extends Command>> commandMap = new HashMap<>();

    /**
     * Constructs a CommandFactory and initializes the command map with
     * available commands.
     */
    public CommandFactory() {
        commandMap.put(Cd.NAME, Cd.class);
        commandMap.put(Exit.NAME, Exit.class);
        commandMap.put(List.NAME, List.class);
        commandMap.put(Mdir.NAME, Mdir.class);
        commandMap.put(Ptime.NAME, Ptime.class);
        commandMap.put(Rdir.NAME, Rdir.class);
        commandMap.put(History.NAME, History.class);
        commandMap.put(Clear.NAME, Clear.class);
    }

    /**
     * Creates a command instance based on the provided command parts.
     *
     * @param commandParts The parts of the command to be executed.
     * @return An instance of the corresponding command class, or a SystemCommand if the command is not recognized.
     */
    public Command createCommand(String[] commandParts) {
        Class<? extends Command> commandClass = commandMap.get(commandParts[0]); // Retrieve command class
        try {
            // Instantiate the command using reflection
            return commandClass.getDeclaredConstructor(String[].class).newInstance((Object) commandParts);
        } catch (Exception ex) {
            // Fallback to SystemCommand if the command class is not found or instantiation fails
            return new SystemCommand(commandParts);
        }
    }
}
