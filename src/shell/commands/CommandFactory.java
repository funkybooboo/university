package shell.commands;

import shell.commands.shellCommands.*;

import java.util.HashMap;
import java.util.Map;

public class CommandFactory {

    private final Map<String, Class<? extends Command>> commandMap = new HashMap<>();

    public CommandFactory() {
        commandMap.put(Cd.name, Cd.class);
        commandMap.put(Exit.name, Exit.class);
        commandMap.put(List.name, List.class);
        commandMap.put(Mdir.name, Mdir.class);
        commandMap.put(Ptime.name, Ptime.class);
        commandMap.put(Rdir.name, Rdir.class);
        commandMap.put(History.name, History.class);
    }

    public Command createCommand(String commandName) {
        Class<? extends Command> commandClass = commandMap.get(commandName.toLowerCase());
        if (commandClass != null) {
            try {
                return commandClass.getDeclaredConstructor().newInstance();
            } catch (Exception e) {
                throw new RuntimeException("Failed to create command instance", e);
            }
        } else {
            return new SystemCommand(commandName);
        }
    }
}
