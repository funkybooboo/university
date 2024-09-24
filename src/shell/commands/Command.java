package shell.commands;

import java.io.InputStream;
import java.io.OutputStream;

public abstract class Command {

    protected final String[] commandParts;

    public Command(String[] commandParts) {
        this.commandParts = commandParts;
    }

    public abstract OutputStream execute(InputStream inputStream, int commandIndex, int commandsLength) throws Exception;
}
