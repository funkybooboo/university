package shell.commands.shellCommands;

import shell.commands.Command;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;

public class Mdir extends Command {
    public static final String NAME = "mdir";

    public Mdir(String[] commandParts) {
        super(commandParts);
    }

    public String getName() {
        return NAME;
    }

    // When the command 'mdir test' is given, a new subdirectory called 'test' should be created.
    // If the directory already exists or the name already exists as a file, an appropriate error message is displayed.
    @Override
    public OutputStream execute(InputStream inputStream) throws Exception {
        if (commandParts.length == 1) {
            throw new Exception("nash: mdir: missing operand");
        }

        for (String directoryName : Arrays.copyOfRange(commandParts, 1, commandParts.length-1)) {
            File dir = new File(directoryName);
            if (dir.exists()) {
                if (dir.isDirectory()) {
                    throw new Exception("nash: mdir: "+directoryName+" directory already exists");
                } else {
                    throw new Exception("nash: mdir: "+directoryName+" file already exists");
                }
            }
            else if (!dir.mkdir()) {
                throw new Exception("nash: mdir: failed to create directory: "+directoryName);
            }
        }
        return new ByteArrayOutputStream();
    }
}
