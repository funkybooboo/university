package shell.commands;

import shell.commands.shellCommands.*;

import java.util.HashMap;
import java.util.Map;

public class CommandFactory {

    private final Map<String, Class<? extends Command>> commandMap = new HashMap<>();

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

    public Command createCommand(String[] commandParts) {
        Class<? extends Command> commandClass = commandMap.get(commandParts[0]);
        try {
            return commandClass.getDeclaredConstructor().newInstance();
        } catch (Exception e) {
            return new SystemCommand(commandParts);
        }
    }
}
