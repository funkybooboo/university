package shell.commands.shellCommands;

public class Ptime implements ShellCommand {
    public static String name = "ptime";
    private static double cumulativeTime = 0.0;

    public static void updateCumulativeTime(double time) {
        cumulativeTime += time;
    }

    @Override
    public void execute(String[] arguments) {
        if (arguments.length > 0) {
            System.err.println("nash: ptime: too many arguments\n");
            return;
        }
        String message = String.format("Total time in child processes: %.4f seconds%n", cumulativeTime);
        System.out.println(message);
    }
}
