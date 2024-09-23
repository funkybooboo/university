package shell;

import sun.misc.Signal;

public class SignalHandler {
    public void setup() {
        Signal.handle(new Signal("INT"), sig -> System.exit(1));
    }
}
