import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * This class provides methods to compute Fibonacci numbers, factorials,
 * and the value of Euler's number 'e' based on command-line arguments.
 *
 * Usage:
 *  java Assign1 -fib [n]
 *  java Assign1 -fac [n]
 *  java Assign1 -e [n]
 */
public class Assign2 {

    private static final long fibMin = 0;
    private static final long fibMax = 40;
    private static final long facMin = 0;
    private static final long facMax = Integer.MAX_VALUE;
    private static final long eMin = 1;
    private static final long eMax = Integer.MAX_VALUE;

    /**
     * Main method to process command-line arguments and perform calculations.
     *
     * @param args Command-line arguments where a number should follow each command.
     */
    public static void main(String[] args) {
        System.out.println();
        if (args.length == 0 || args.length % 2 != 0) {
            printHelpMessage();
            return;
        }
        for (int i = 0; i < args.length; i += 2) {
            String command = args[i];
            Long n;
            switch (command) {
                case "-fib":
                    n = getN(args, i);
                    if (n == null) {
                        return;
                    }
                    if (n < fibMin || n > fibMax) {
                        System.out.println("Fibonacci valid range is [" + fibMin + ", " + fibMax + "]");
                        break;
                    }
                    System.out.println("Fibonacci of " + n + " is " + fibonacci(Math.toIntExact(n)));
                    break;
                case "-fac":
                    n = getN(args, i);
                    if (n == null) {
                        return;
                    }
                    if (n < facMin || n > facMax) {
                        System.out.println("Factorial valid range is [" + facMin + ", " + facMax + "]");
                        break;
                    }
                    System.out.println("Factorial of " + n + " is " + factorial(Math.toIntExact(n)));
                    break;
                case "-e":
                    n = getN(args, i);
                    if (n == null) return;
                    if (n < eMin || n > eMax) {
                        System.out.println("E valid range is [" + eMin + ", " + eMax + "]");
                        break;
                    }
                    System.out.println("Value of e using " + n + " iterations is " + e(Math.toIntExact(n)));
                    break;
                default:
                    printInvalidLineArgumentMessage(command);
                    return;
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
    private static Long getN(String[] args, int i) {
        try {
            return Long.parseLong(args[i + 1]);
        } catch (NumberFormatException ex) {
            printInvalidLineArgumentMessage(args[i + 1]);
            return null;
        }
    }

    /**
     * Prints a message for an invalid command-line argument.
     *
     * @param arg The invalid argument.
     */
    private static void printInvalidLineArgumentMessage(String arg) {
        System.out.println("Unknown command line argument: " + arg);
    }

    /**
     * Prints the help message for the program.
     */
    private static void printHelpMessage() {
        System.out.println("--- Assign 1 Help ---");
        System.out.println("  -fib [n] : Compute the Fibonacci of [n]; valid range [" + fibMin + ", " + fibMax + "]");
        System.out.println("  -fac [n] : Compute the Factorial of [n]; valid range [" + facMin + ", " + facMax + "]");
        System.out.println("  -e   [n] : Compute the value of 'e' using [n] iterations; valid range [" + eMin + ", " + eMax + "]");
    }

    /**
     * Computes the Fibonacci number for a given non-negative integer.
     *
     * @param n The index of the Fibonacci sequence (non-negative).
     * @return The Fibonacci number at index n.
     */
    private static int fibonacci(int n) {
        if (n == 0) {
            return 0;
        }
        if (n == 1) {
            return 1;
        }
        int[] fib = new int[n + 1];
        fib[0] = 0;
        fib[1] = 1;
        for (int i = 2; i <= n; i++) {
            fib[i] = fib[i - 1] + fib[i - 2];
        }
        return fib[n];
    }

    /**
     * Computes the factorial of a given non-negative integer.
     *
     * @param n The non-negative integer to compute the factorial of.
     * @return The factorial of the integer n.
     */
    private static BigInteger factorial(int n) {
        BigInteger result = BigInteger.ONE;
        for (int i = 2; i <= n; i++) {
            result = result.multiply(BigInteger.valueOf(i));
        }
        return result;
    }

    /**
     * Computes the value of Euler's number 'e' using a specified number of iterations.
     *
     * @param n The number of iterations to use in the calculation (must be >= 1).
     * @return The value of 'e' calculated to 16 decimal places.
     */
    public static BigDecimal e(int n) {
        BigDecimal e = BigDecimal.ZERO;
        BigDecimal divisor = BigDecimal.ONE;
        for (int i = 0; i <= n; i++) {
            if (i > 0) {
                divisor = divisor.multiply(BigDecimal.valueOf(i));
            }
            e = e.add(BigDecimal.ONE.divide(divisor, 16, RoundingMode.FLOOR));
        }
        return e;
    }
}
