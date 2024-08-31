import calculable.*;
import java.util.Collection;

/**
 * The {@code Help} class provides utility methods for displaying help messages
 * and valid ranges for command-line arguments.
 * It helps users by providing information on unknown arguments and usage details for supported commands.
 *
 * @author Nate Stott
 */
public class Help {

    /**
     * Prints an error message indicating an unknown command-line argument.
     * <p>
     * This method is used to inform users when an argument provided to the
     * command-line application is not recognized.
     * </p>
     *
     * @param flag The unknown command-line argument.
     */
    public static void printUnknown(String flag) {
        System.out.println("Unknown command line argument: " + flag);
    }

    /**
     * Prints the help message for the program, including usage information
     * for all supported commands.
     * <p>
     * This method retrieves a collection of all {@code Calculable} instances
     * from the {@code CalculableFactory} and prints their usage messages, which
     * describe how to use each command.
     * </p>
     */
    public static void printUsage() {
        System.out.println("--- Assign 2 Help ---");
        Collection<Calculable<?>> collection = CalculableFactory.getCalculablesAll();
        for (Calculable<?> calculable : collection) {
            System.out.println(calculable.getUsageMessage());
        }
    }
}
