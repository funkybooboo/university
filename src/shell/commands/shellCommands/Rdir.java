package shell.commands.shellCommands;

import shell.commands.Command;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;

public class Rdir extends Command {
    public static final String NAME = "rdir";

    public Rdir(String[] commandParts) {
        super(commandParts);
    }

    public String getName() {
        return NAME;
    }

    // When the command 'rdir test' is given, if the folder 'test' exists, it is removed.
    // If the directory didn't exist, an error messages is displayed.
    @Override
    public OutputStream execute(InputStream inputStream) throws Exception {
        if (commandParts.length == 1) {
            throw new Exception("nash: rdir: missing operand");
        }

        for (String directoryName : Arrays.copyOfRange(commandParts, 1, commandParts.length-1)) {
            File dir = new File(directoryName);

            if (dir.exists()) {
                if (dir.isDirectory()) {
                    if (!dir.delete()) {
                        throw new Exception("nash: rdir: failed to remove directory: "+directoryName);
                    }
                } else {
                    throw new Exception("nash: rdir: "+directoryName+" is not a directory");
                }
            } else {
                throw new Exception("nash: rdir: directory "+directoryName+" does not exist");
            }
        }
        return new ByteArrayOutputStream();
    }
}
