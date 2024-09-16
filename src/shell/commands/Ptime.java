package shell.commands;

public class Ptime implements Command {
    private static double cumulativeTime = 0.0;

    public static void updateCumulativeTime(double time) {
        cumulativeTime += time;
    }

    @Override
    public Result execute(String[] arguments) {
        String message = String.format("Cumulative time spent: %.4f seconds%n", cumulativeTime);
        return new Result(message, true);
    }
}
