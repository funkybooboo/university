package shell;

import shell.commands.Pipeline;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.LinkedList;
import java.util.Optional;

public class CommandExecutor {

    private final Pipeline pipeline = new Pipeline();

    public void execute(LinkedList<String[]> commandStack, boolean isBackground) {
        if (isBackground) {
            executeInBackground(commandStack);
        } else {
            executeInForeground(commandStack);
        }
    }

    public void executeInForeground(LinkedList<String[]> commandStack) {
        execute(commandStack);
    }

    private void executeInBackground(LinkedList<String[]> commandStack) {
        new Thread(() -> execute(commandStack)).start();
    }

    private void execute(LinkedList<String[]> commandStack) {
        try {
            printOutputStream(pipeline.execute(commandStack));
        } catch (Exception ex) {
            handleException(ex);
        }
    }

    public void handleException(Exception ex) {
        System.err.println(Optional.ofNullable(ex.getMessage()).orElse(""));
    }

    private void printOutputStream(OutputStream outputStream) throws IOException {
        String output;
        if (outputStream instanceof ByteArrayOutputStream byteArrayOutputStream) {
            output = byteArrayOutputStream.toString();
        } else {
            throw new IOException("nash: need to implement OutputStream subtype output");
        }
        System.out.print(output);
    }
}
