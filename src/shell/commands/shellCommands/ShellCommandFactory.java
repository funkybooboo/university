package shell.commands.shellCommands;

import java.util.HashMap;
import java.util.Map;

public class ShellCommandFactory {

    private final Map<String, Class<? extends ShellCommand>> commandMap = new HashMap<>();

    public ShellCommandFactory() {
        commandMap.put(Cd.name, Cd.class);
        commandMap.put(Exit.name, Exit.class);
        commandMap.put(List.name, List.class);
        commandMap.put(Mdir.name, Mdir.class);
        commandMap.put(Ptime.name, Ptime.class);
        commandMap.put(Rdir.name, Rdir.class);
        commandMap.put(History.name, History.class);
    }

    public ShellCommand createCommand(String commandName) {
        Class<? extends ShellCommand> commandClass = commandMap.get(commandName.toLowerCase());
        try {
            return commandClass.getDeclaredConstructor().newInstance();
        } catch (Exception e) {
            throw new RuntimeException("Failed to create command instance", e);
        }
    }
}
