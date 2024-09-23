package shell.commands.shellCommands;

import shell.commands.Command;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

public class Exit extends Command {
    public static final String NAME = "exit";

    public Exit(String[] commandParts) {
        super(commandParts);
    }

    // The command 'exit' is used to terminate your shell program, when entered, your shell program should end.
    @Override
    public OutputStream execute(InputStream inputStream) throws Exception {
        if (commandParts.length == 1) {
            System.exit(0);
        }
        if (commandParts.length == 2) {
            try {
                System.exit(Integer.parseInt(commandParts[1]));
            }
            catch (Exception e) {
                throw new Exception("nash: "+NAME+": invalid exit code");
            }
        }
        else {
            throw new Exception("nash: "+NAME+": too many arguments");
        }
        return new ByteArrayOutputStream();
    }
}
