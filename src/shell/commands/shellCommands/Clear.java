package shell.commands.shellCommands;

import shell.commands.Command;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

public class Clear extends Command {
    public static final String NAME = "clear";

    public Clear(String[] commandParts) {
        super(commandParts);
    }

    public String getName() {
        return NAME;
    }

    @Override
    public OutputStream execute(InputStream inputStream) throws Exception {
        if (commandParts.length > 1) {
            throw new Exception("nash: clear: invalid number of arguments");
        }

        String os = System.getProperty("os.name").toLowerCase();
        try {
            if (os.contains("win")) {
                new ProcessBuilder("cmd", "/c", "cls").inheritIO().start().waitFor();
            }
            else {
                System.out.print("\033[H\033[2J"); // Clear the screen
                System.out.flush(); // Flush output
                System.out.print("\033[1;1H"); // Move cursor to top left
            }
        } catch (Exception e) {
            throw new Exception("nash: clear: error clearing screen: " + e.getMessage());
        }
        return new ByteArrayOutputStream();
    }

}
