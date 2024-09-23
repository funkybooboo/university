package shell.commands;

import java.io.InputStream;
import java.io.OutputStream;

public abstract class Command {

    protected final String[] commandParts;

    public Command(String[] commandParts) {
        this.commandParts = commandParts;
    }

    protected abstract String getName();

    public abstract OutputStream execute(InputStream inputStream) throws Exception;
}
