package shell.commands.shellCommands;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;

public class List implements ShellCommand {
    public static String name = "list";
    // - The first four characters indicate: directory, user can read, user can write, user can execute.
    //  Use the external 'ls -l' command on Linux for examples of the output.
    //  Your shell only needs to display these details for the current user.
    // - The next 10 characters contains the size of the file in bytes, right justified (no commas).
    // - The next field is the date of last modification for the file; follow the example formatting.
    // - The last field is the name of the file.
    @Override
    public void execute(String[] arguments) {
        // Determine the starting directory
        String start = (arguments.length == 0) ? "." : arguments[0];

        // Get the directory
        File directory = new File(start);

        // Check if the directory exists and is actually a directory
        if (!directory.exists() || !directory.isDirectory()) {
            System.err.println("nash: list: no such file or directory");
            return;
        }

        // List all files and directories in the directory
        File[] files = directory.listFiles();
        if (files == null) {
            System.err.println("nash: list: unable to access the directory");
            return;
        }

        // Prepare output
        StringBuilder output = new StringBuilder();
        for (File file : files) {
            String permissions = getPermissions(file);
            String size = String.format("%10d", file.length());
            String modificationDate = getModificationDate(file);
            String name = file.getName();
            output.append(String.format("%s %s %s %s%n", permissions, size, modificationDate, name));
        }
        System.out.println(output);
    }

    private String getPermissions(File file) {
        return String.valueOf(file.isDirectory() ? 'd' : '-') +
                // Check read permission
                (file.canRead() ? 'r' : '-') +
                // Check write permission
                (file.canWrite() ? 'w' : '-') +
                // Check execute permission
                (file.canExecute() ? 'x' : '-');
    }

    private String getModificationDate(File file) {
        SimpleDateFormat dateFormat = new SimpleDateFormat("MMM dd yyyy hh:mm");
        Date lastModified = new Date(file.lastModified());
        return dateFormat.format(lastModified);
    }
}
