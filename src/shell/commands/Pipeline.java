package shell.commands;

import java.io.*;
import java.util.*;

public class Pipeline {
    private final CommandFactory commandFactory = new CommandFactory();

    public OutputStream execute(LinkedList<String[]> commandStack) throws Exception {
        if (commandStack.isEmpty()) {
            return new ByteArrayOutputStream();
        }

        Command[] commands = getCommands(commandStack);

        OutputStream[] outputStreams = new OutputStream[commands.length];

        Command firstCommand = commands[0];
        outputStreams[0] = firstCommand.execute(new ByteArrayInputStream(new byte[0]), 0, commands.length);

        for (int i = 1; i < commands.length; i++) {
            outputStreams[i] = commands[i].execute(outputStreamToInputStream(outputStreams[i-1]), i, commands.length);
        }

        return outputStreams[outputStreams.length-1];
    }

    private InputStream outputStreamToInputStream(OutputStream outputStream) throws IOException {
        InputStream inputStream;
        if (outputStream instanceof ByteArrayOutputStream byteArrayOutputStream) {
            inputStream = new ByteArrayInputStream(byteArrayOutputStream.toByteArray());
        }
        else {
            throw new IOException("nash: need to implement OutputSteam subtype conversion");
        }
        return inputStream;
    }

    private Command[] getCommands(LinkedList<String[]> commandStack) {
        Command[] commands = new Command[commandStack.size()];

        int index = 0;
        while (!commandStack.isEmpty()) {
            commands[index++] = commandFactory.createCommand(commandStack.poll());
        }
        return commands;
    }
}
