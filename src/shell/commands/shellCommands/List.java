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

    @Override
    public OutputStream execute(InputStream inputStream, int commandIndex, int commandsLength) throws Exception {
        if (commandParts.length > 2) {
            throw new Exception("nash: " + NAME + ": invalid number of arguments");
        }

        // Determine the starting directory
        String start = (commandParts.length == 1) ? "." : commandParts[1];
        File targetDirectoryPath;

        // Resolve the path
        if (!new File(start).isAbsolute()) {
            String currentDirectoryPath = System.getProperty("user.dir");
            targetDirectoryPath = new File(currentDirectoryPath, start).getCanonicalFile();
        }
        else {
            targetDirectoryPath = new File(start).getCanonicalFile();
        }

        // Check if the directory exists and is actually a directory
        if (!targetDirectoryPath.exists() || !targetDirectoryPath.isDirectory()) {
            throw new Exception("nash: " + NAME + ": no such file or directory");
        }

        // List all files and directories in the directory
        File[] files = targetDirectoryPath.listFiles();
        if (files == null) {
            throw new Exception("nash: " + NAME + ": unable to access the directory");
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
                (file.canRead() ? 'r' : '-') +
                (file.canWrite() ? 'w' : '-') +
                (file.canExecute() ? 'x' : '-');
    }

    private String getModificationDate(File file) {
        SimpleDateFormat dateFormat = new SimpleDateFormat("MMM dd yyyy hh:mm");
        Date lastModified = new Date(file.lastModified());
        return dateFormat.format(lastModified);
    }
}
