package calculable;

/**
 * The {@code Fib} class implements the {@code Calculable} interface to compute
 * the Fibonacci number at a given index in the Fibonacci sequence.
 * This class calculates Fibonacci numbers up to a maximum index of 40.
 *
 * @author Nate Stott
 */
public class Fib implements Calculable<Integer> {

    // Constants for the valid range of input values
    public static final long MIN = 0;
    public static final long MAX = 40;

    /**
     * Checks if the given index is within the valid range for Fibonacci calculation.
     * <p>
     * The valid range for the Fibonacci index is from {@code MIN} to {@code MAX}.
     * </p>
     *
     * @param n the index to check
     * @return {@code true} if the index is out of bounds, {@code false} otherwise
     */
    @Override
    public boolean isOutOfBounds(long n) {
        return n < MIN || n > MAX;
    }

    /**
     * Computes the Fibonacci number at a given index in the sequence.
     * <p>
     * The Fibonacci sequence is defined as follows:
     * <pre>
     *     F(0) = 0
     *     F(1) = 1
     *     F(n) = F(n-1) + F(n-2) for n &gt; 1
     * </pre>
     * The method uses an iterative approach to compute the Fibonacci number.
     * </p>
     *
     * @param n The index of the Fibonacci sequence (non-negative).
     * @return The Fibonacci number at index {@code n}.
     */
    @Override
    public Integer calculate(long n) {
        if (n == 0) {
            return 0;
        }
        if (n == 1) {
            return 1;
        }
        int[] fib = new int[(int) (n + 1)];
        fib[0] = 0;
        fib[1] = 1;
        for (int i = 2; i <= n; i++) {
            fib[i] = fib[i - 1] + fib[i - 2];
        }
        return fib[(int) n];
    }

    /**
     * Generates a message displaying the Fibonacci number at the specified index.
     * <p>
     * The message includes the index and the corresponding Fibonacci number.
     * </p>
     *
     * @param n The index of the Fibonacci sequence.
     * @param integer The Fibonacci number at the specified index.
     * @return A formatted string with the result message.
     */
    @Override
    public String getResultMessage(long n, Object integer) {
        return "Fibonacci of " + n + " is " + integer;
    }

    /**
     * Provides a message describing the valid range of indices for Fibonacci calculation.
     * <p>
     * This message informs users about the acceptable range for the index {@code n}.
     * </p>
     *
     * @return A string describing the range of valid indices.
     */
    @Override
    public String getRangeMessage() {
        return "Fibonacci valid range is [" + MIN + ", " + MAX + "]";
    }

    /**
     * Provides usage information for the command associated with this calculation.
     * <p>
     * This message guides users on how to use the command to compute the Fibonacci number
     * at a specified index.
     * </p>
     *
     * @return A string with the usage message.
     */
    @Override
    public String getUsageMessage() {
        return "  -fib [n] : Compute the Fibonacci of [n]; valid range [" + MIN + ", " + MAX + "]";
    }
}
