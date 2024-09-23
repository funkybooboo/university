package shell.commands.shellCommands;

import shell.commands.Command;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.text.SimpleDateFormat;
import java.util.Date;

public class List extends Command {
    public static final String NAME = "list";

    public List(String[] commandParts) {
        super(commandParts);
    }

    // - The first four characters indicate: directory, user can read, user can write, user can execute.
    //  Use the external 'ls -l' command on Linux for examples of the output.
    //  Your shell only needs to display these details for the current user.
    // - The next 10 characters contains the size of the file in bytes, right justified (no commas).
    // - The next field is the date of last modification for the file; follow the example formatting.
    // - The last field is the name of the file.
    @Override
    public OutputStream execute(InputStream inputStream) throws Exception {
        if (commandParts.length > 2) {
            throw new Exception("nash: "+NAME+": invalid number of arguments");
        }
        // Determine the starting directory
        String start = (commandParts.length == 1) ? "." : commandParts[1];

        // Get the directory
        File directory = new File(start);

        // Check if the directory exists and is actually a directory
        if (!directory.exists() || !directory.isDirectory()) {
            throw new Exception("nash: "+NAME+": no such file or directory");
        }

        // List all files and directories in the directory
        File[] files = directory.listFiles();
        if (files == null) {
            throw new Exception("nash: "+NAME+": unable to access the directory");
        }

        // Prepare output
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        for (File file : files) {
            String permissions = getPermissions(file);
            String size = String.format("%10d", file.length());
            String modificationDate = getModificationDate(file);
            String name = file.getName();
            output.write(String.format("%s %s %s %s%n", permissions, size, modificationDate, name).getBytes());
        }
        return output;
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
