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

        return execute(commands);
    }

    private OutputStream execute(Command[] commands) throws Exception {

        OutputStream[] outputStreams = new OutputStream[commands.length];

        Command firstCommand = commands[0];
        outputStreams[0] = firstCommand.execute(new ByteArrayInputStream(new byte[0]));

        for (int i = 1; i < commands.length; i++) {
            outputStreams[i] = commands[i].execute(outputStreamToInputStream(outputStreams[i-1]));

            try (InputStream in = outputStreamToInputStream(outputStreams[i - 1]);
                 OutputStream out = outputStreams[i]) {
                byte[] buffer = new byte[1024];
                int bytesRead;
                while ((bytesRead = in.read(buffer)) != -1) {
                    out.write(buffer, 0, bytesRead);
                }
                out.flush();
            } catch (IOException ex) {
                throw new Exception("nash: pipe: error while piping data", ex);
            }
        }
        return outputStreams[outputStreams.length-1];
    }

    private InputStream outputStreamToInputStream(OutputStream outputStream) throws IOException {
        // TODO is this bad? prob
        if (!(outputStream instanceof ByteArrayOutputStream byteArrayOutputStream)) {
            throw new IllegalArgumentException("nash: pipeline: OutputStream must be an instance of ByteArrayOutputStream");
        }
        byte[] byteArray = byteArrayOutputStream.toByteArray();
        return new ByteArrayInputStream(byteArray);
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
