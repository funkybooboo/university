package shell.commands.shellCommands;

import shell.commands.Command;
import shell.commands.Result;

import java.io.File;

public class Mdir implements Command {
    public static String name = "mdir";
    // When the command 'mdir test' is given, a new subdirectory called 'test' should be created.
    // If the directory already exists or the name already exists as a file, an appropriate error message is displayed.
    @Override
    public Result execute(String[] arguments, String previousOutput) {
        if (arguments.length == 0) {
            return new Result("nash: mdir: missing operand", false);
        }

        StringBuilder output = new StringBuilder();
        boolean success = true;

        for (String directoryName : arguments) {
            File dir = new File(directoryName);
            if (dir.exists()) {
                if (dir.isDirectory()) {
                    output.append("nash: mdir: ").append(directoryName).append(" directory already exists\n");
                } else {
                    output.append("nash: mdir: ").append(directoryName).append(" file already exists\n");
                }
                success = false;
            }
            else if (!dir.mkdir()) {
                output.append("nash: mdir: failed to create directory ").append(directoryName).append("\n");
                success = false;
            }
        }

        return new Result(output.toString(), success);
    }

}
