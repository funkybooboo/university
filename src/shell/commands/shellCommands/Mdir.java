package shell.commands.shellCommands;

import shell.commands.Command;

import java.io.File;

public class Mdir implements Command {
    public static String name = "mdir";
    // When the command 'mdir test' is given, a new subdirectory called 'test' should be created.
    // If the directory already exists or the name already exists as a file, an appropriate error message is displayed.
    @Override
    public void execute(String[] arguments) {
        if (arguments.length == 0) {
            System.err.println("nash: mdir: missing operand");
            return;
        }

        for (String directoryName : arguments) {
            File dir = new File(directoryName);
            if (dir.exists()) {
                if (dir.isDirectory()) {
                    System.err.println("nash: mdir: "+directoryName+" directory already exists");
                } else {
                    System.err.println("nash: mdir: "+directoryName+" file already exists");
                }
            }
            else if (!dir.mkdir()) {
                System.err.println("nash: mdir: failed to create directory: "+directoryName);
            }
        }
    }
}
