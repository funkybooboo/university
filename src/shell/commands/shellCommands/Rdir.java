package shell.commands.shellCommands;

import shell.commands.Command;
import shell.commands.Result;

import java.io.File;

public class Rdir implements Command {
    public static String name = "rdir";
    // When the command 'rdir test' is given, if the folder 'test' exists, it is removed.
    // If the directory didn't exist, an error messages is displayed.
    @Override
    public Result execute(String[] arguments, String previousOutput) {
        if (arguments.length == 0) {
            return new Result("nash: rdir: missing operand", false);
        }

        StringBuilder output = new StringBuilder();
        boolean success = true;

        for (String directoryName : arguments) {
            File dir = new File(directoryName);

            if (dir.exists()) {
                if (dir.isDirectory()) {
                    if (!dir.delete()) {
                        output.append("nash: rdir: failed to remove directory ").append(directoryName).append("\n");
                        success = false;
                    }
                } else {
                    output.append("nash: rdir: ").append(directoryName).append(" is not a directory\n");
                    success = false;
                }
            } else {
                output.append("nash: rdir: directory ").append(directoryName).append(" does not exist\n");
                success = false;
            }
        }

        return new Result(output.toString(), success);
    }

}
