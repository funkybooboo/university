package shell.commands.shellCommands;

import java.io.File;

public class Rdir implements ShellCommand {
    public static String name = "rdir";
    // When the command 'rdir test' is given, if the folder 'test' exists, it is removed.
    // If the directory didn't exist, an error messages is displayed.
    @Override
    public void execute(String[] arguments) {
        if (arguments.length == 0) {
            System.err.println("nash: rdir: missing operand");
            return;
        }

        for (String directoryName : arguments) {
            File dir = new File(directoryName);

            if (dir.exists()) {
                if (dir.isDirectory()) {
                    if (!dir.delete()) {
                        System.err.println("nash: rdir: failed to remove directory: "+directoryName);
                    }
                } else {
                    System.err.println("nash: rdir: "+directoryName+" is not a directory");
                }
            } else {
                System.err.println("nash: rdir: directory "+directoryName+" does not exist");
            }
        }
    }
}
