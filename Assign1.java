import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

public class Assign1 {

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
                    if (n == null) return;
                    if (n < 0 || n > 40) {
                        System.out.println("Fibonacci valid range is [0, 40]");
                        break;
                    }
                    System.out.println("Fibonacci of " + n + " is " + fibonacci(Math.toIntExact(n)));
                    break;
                case "-fac":
                    n = getN(args, i);
                    if (n == null) return;
                    if (n < 0 || n > 2147483647) {
                        System.out.println("Factorial valid range is [0, 2147483647]");
                        break;
                    }
                    System.out.println("Factorial of " + n + " is " + factorial(Math.toIntExact(n)));
                    break;
                case "-e":
                    n = getN(args, i);
                    if (n == null) return;
                    if (n < 1 || n > 2147483647) {
                        System.out.println("E valid range is [1, 2147483647]");
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

    private static Long getN(String[] args, int i) {
        try {
            return Long.parseLong(args[i + 1]);
        } catch (NumberFormatException ex) {
            printInvalidLineArgumentMessage(args[i + 1]);
            return null;
        }
    }

    private static void printInvalidLineArgumentMessage(String arg) {
        System.out.println("Unknown command line argument: " + arg);
    }

    private static void printHelpMessage() {
        System.out.println("--- Assign 1 Help ---");
        System.out.println("  -fib [n] : Compute the Fibonacci of [n]; valid range [0, 40]");
        System.out.println("  -fac [n] : Compute the Factorial of [n]; valid range [0, 2147483647]");
        System.out.println("  -e   [n] : Compute the value of 'e' using [n] iterations; valid range [1, 2147483647]");
    }

    private static int fibonacci(int n) {
        if (n == 0) return 0;
        if (n == 1) return 1;
        int[] fib = new int[n + 1];
        fib[0] = 0;
        fib[1] = 1;
        for (int i = 2; i <= n; i++) {
            fib[i] = fib[i - 1] + fib[i - 2];
        }
        return fib[n];
    }

    private static BigInteger factorial(int n) {
        BigInteger result = BigInteger.ONE;
        for (int i = 2; i <= n; i++) {
            result = result.multiply(BigInteger.valueOf(i));
        }
        return result;
    }

    public static BigDecimal e(int n) {
        BigDecimal e = BigDecimal.ZERO;
        BigDecimal divisor = BigDecimal.ONE;
        for (int i = 0; i <= n; i++) {
            if (i > 0) divisor = divisor.multiply(BigDecimal.valueOf(i));
            e = e.add(BigDecimal.ONE.divide(divisor, 16, RoundingMode.FLOOR));
        }
        return e;
    }
}
