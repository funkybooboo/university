package shell;

import sun.misc.Signal;

/**
 * The SignalHandler class manages system signals for the shell application.
 * It sets up handlers for signals like INT (interrupt).
 *
 * @author Nate Stott
 */
public class SignalHandler {

    /**
     * Sets up the signal handlers for the shell.
     * Currently, it handles the INT signal to terminate the application.
     */
    public void setup() {
        Signal.handle(new Signal("INT"), sig -> System.exit(1)); // Handle interrupt signal (Control-C)
    }
}
