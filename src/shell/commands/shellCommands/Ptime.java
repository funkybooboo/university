package shell.commands.shellCommands;

import shell.commands.Command;
import shell.commands.Result;

public class Ptime implements Command {
    private static double cumulativeTime = 0.0;

    public static void updateCumulativeTime(double time) {
        cumulativeTime += time;
    }

    @Override
    public Result execute(String[] arguments, String previousOutput) {
        String message = String.format("Cumulative time spent: %.4f seconds%n", cumulativeTime);
        return new Result(message, true);
    }
}
