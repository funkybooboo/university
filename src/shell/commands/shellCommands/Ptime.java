package shell.commands.shellCommands;

import shell.commands.Command;
import shell.commands.Result;

public class Ptime implements Command {
    public static String name = "ptime";
    private static double cumulativeTime = 0.0;

    public static void updateCumulativeTime(double time) {
        cumulativeTime += time;
    }

    @Override
    public Result execute(String[] arguments, String previousOutput) {
        String message = String.format("Total time in child processes: %.4f seconds%n", cumulativeTime);
        return new Result(message, true);
    }
}
