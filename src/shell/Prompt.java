package shell;

import java.io.IOException;
import java.net.InetAddress;
import java.util.Scanner;

public class Prompt {

    public String getUserCommand() throws IOException {
        String username = System.getProperty("user.name");
        String hostname = InetAddress.getLocalHost().getHostName();
        String currentDir = System.getProperty("user.dir");
        String prompt = String.format("%s@%s:%s$ ", username, hostname, currentDir);
        System.out.print(prompt);
        try (Scanner scanner = new Scanner(System.in)) {
            return scanner.nextLine();
        }
    }
}
