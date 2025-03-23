import calculable.Calculable;
import calculable.CalculableFactory;

/**
 * The {@code Assn2} class processes command-line arguments to perform calculations
 * using instances of {@code Calculable} obtained from the {@code CalculableFactory}.
 * It validates inputs and outputs the results or errors based on the provided arguments.
 * <p>
 * Usage:
 * <pre>
 *     gradle run --args="<flag1> <number1> <flag2> <number2> ..."
 * </pre>
 * </p>
 * The <flag> should be a valid command recognized by the {@code CalculableFactory},
 * and <number> should be a valid long integer.
 *
 * @author Nate Stott
 */
public class Assn2 {

    /**
     * The main method that processes command-line arguments and performs calculations.
     * <p>
     * It expects pairs of arguments where each pair consists of a flag and a number.
     * The method retrieves the appropriate {@code Calculable} instance based on the flag,
     * performs the calculation, and prints the result or an error message if the input is invalid.
     * </p>
     *
     * @param args Command-line arguments where a number should follow each flag.
     *             The number must be a valid long integer.
     */
    public static void main(String[] args) {
        System.out.println();
        if (args.length == 0 || args.length % 2 != 0) {
            Help.printUsage();
            return;
        }

        for (int i = 0; i < args.length; i += 2) {
            String flag = args[i];
            Calculable<?> calculable = CalculableFactory.getCalculable(flag);
            if (calculable == null) {
                Help.printUnknown(flag);
                break;
            }

            Long n = parseNumber(args[i + 1]);
            if (n == null) {
                Help.printUnknown(args[i + 1]);
                break;
            }

            if (calculable.isOutOfBounds(n)) {
                System.out.println(calculable.getRangeMessage());
            } else {
                System.out.println(calculable.getResultMessage(n, calculable.calculate(n)));
            }
        }
    }

    /**
     * Parses the given argument into a {@code Long}.
     * <p>
     * If the argument cannot be parsed into a long integer, this method returns {@code null}.
     * </p>
     *
     * @param arg The argument to parse.
     * @return The parsed {@code Long} value or {@code null} if parsing fails.
     */
    private static Long parseNumber(String arg) {
        try {
            return Long.parseLong(arg);
        } catch (NumberFormatException ex) {
            return null;
        }
    }
}
