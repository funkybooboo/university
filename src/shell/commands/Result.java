package shell.commands;

public class Result {
    private final String output;
    private final boolean isCommand;
    private final boolean isSuccess;

    public Result() {
        output = "";
        isSuccess = true;
        isCommand = false;
    }

    public Result(String output, boolean isSuccess) {
        this.output = output;
        this.isSuccess = isSuccess;
        this.isCommand = false;
    }

    public Result(String output, boolean isSuccess, boolean isCommand) {
        this.output = output;
        this.isSuccess = isSuccess;
        this.isCommand = isCommand;
    }

    public String getOutput() {
        return output;
    }

    public boolean isSuccess() {
        return isSuccess;
    }

    public boolean isCommand() {
        return isCommand;
    }
}
