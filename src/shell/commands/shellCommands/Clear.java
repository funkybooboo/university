package shell.commands.shellCommands;

import shell.commands.Command;

public class Clear implements Command {
    public static String name = "clear";

    @Override
    public void execute(String[] arguments) {
        if (arguments.length > 0) {
            System.err.println("nash: clear: invalid number of arguments");
            return;
        }

        String os = System.getProperty("os.name").toLowerCase();
        try {
            if (os.contains("win")) {
                new ProcessBuilder("cmd", "/c", "cls").inheritIO().start().waitFor();
            } else {
                System.out.print("\033[H\033[2J"); // Clear the screen
                System.out.flush(); // Flush output
                System.out.print("\033[1;1H"); // Move cursor to top left
            }
        } catch (Exception e) {
            System.err.println("nash: clear: error clearing screen: " + e.getMessage());
        }
    }

}
