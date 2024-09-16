package shell.commands;

public class Result {
    private final String output;
    private final boolean isSuccess;

    public Result() {
        output = "";
        isSuccess = true;
    }

    public Result(String output, boolean isSuccess) {
        this.output = output;
        this.isSuccess = isSuccess;
    }

    public String getOutput() {
        return output;
    }

    public boolean isSuccess() {
        return isSuccess;
    }
}
