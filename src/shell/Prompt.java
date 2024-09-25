package shell;

import java.util.Scanner;

public class Prompt {
    private final Scanner scanner = new Scanner(System.in);
    public String getUserCommand() {
        String currentDirPath = System.getProperty("user.dir");
        String prompt = String.format("[%s]: ", currentDirPath);
        System.out.print(prompt);
        return scanner.nextLine();
    }
}
