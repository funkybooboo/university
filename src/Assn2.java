import Calculable.Calculable;
import Calculable.CalculableFactory;

public class Assn2 {

    private static final CalculableFactory factory = new CalculableFactory();

    /**
     * Main method to process command-line arguments and perform calculations.
     *
     * @param args Command-line arguments where a number should follow each command.
     */
    public static void main(String[] args) {
        System.out.println();
        if (args.length == 0 || args.length % 2 != 0) {
            Help.printUsage();
            return;
        }

        for (int i = 0; i < args.length; i += 2) {
            String command = args[i];
            Long n = getNOrPrintUnknown(args, i);
            if (n == null) {
                return;
            }

            Calculable<?> calculable = factory.createCalculable(command);
            if (calculable.bounds(n)) {
                calculable.printAnswer(n, calculable.calculate(n));
            }
            else {
                calculable.printRange();
            }
        }
    }

    /**
     * Parses the number argument following the command.
     *
     * @param args The command-line arguments.
     * @param i The index of the command in the arguments array.
     * @return The parsed number or null if parsing fails.
     */
    private static Long getNOrPrintUnknown(String[] args, int i) {
        try {
            return Long.parseLong(args[i + 1]);
        } catch (NumberFormatException ex) {
            Help.printUnknown(args[i + 1]);
            return null;
        }
    }
}
