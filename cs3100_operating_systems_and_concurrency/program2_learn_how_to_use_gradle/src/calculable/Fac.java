package calculable;

import java.math.BigInteger;

/**
 * The {@code Fac} class implements the {@code Calculable} interface to compute
 * the factorial of a given non-negative integer.
 * Factorials are computed using the product of all positive integers up to the given integer.
 *
 * @author Nate Stott
 */
public class Fac implements Calculable<BigInteger> {

    // Constants for the valid range of input values
    public static final long MIN = 0;
    public static final long MAX = Integer.MAX_VALUE;

    /**
     * Checks if the given number is within the valid range for factorial calculation.
     * <p>
     * The valid range for the factorial input is from {@code MIN} to {@code MAX}.
     * </p>
     *
     * @param n the number to check
     * @return {@code true} if the number is out of bounds, {@code false} otherwise
     */
    @Override
    public boolean isOutOfBounds(long n) {
        return n < MIN || n > MAX;
    }

    /**
     * Computes the factorial of the given non-negative integer.
     * <p>
     * The factorial is calculated as the product of all positive integers up to {@code n}.
     * For example, the factorial of 5 (denoted as 5!) is 120.
     * </p>
     *
     * @param n The non-negative integer for which to compute the factorial.
     * @return The factorial of the integer {@code n} as a {@code BigInteger}.
     */
    @Override
    public BigInteger calculate(long n) {
        BigInteger result = BigInteger.ONE;
        for (int i = 2; i <= n; i++) {
            result = result.multiply(BigInteger.valueOf(i));
        }
        return result;
    }

    /**
     * Generates a message displaying the result of the factorial calculation.
     * <p>
     * The message includes the input integer {@code n} and its factorial result.
     * </p>
     *
     * @param n The non-negative integer used to calculate the factorial.
     * @param bigInteger The result of the factorial calculation.
     * @return A formatted string with the result message.
     */
    @Override
    public String getResultMessage(long n, Object bigInteger) {
        return "Factorial of " + n + " is " + bigInteger;
    }

    /**
     * Provides a message describing the valid range of values for factorial calculation.
     * <p>
     * This message informs users about the acceptable range for the input value {@code n}.
     * </p>
     *
     * @return A string describing the range of valid values.
     */
    @Override
    public String getRangeMessage() {
        return "Factorial valid range is [" + MIN + ", " + MAX + "]";
    }

    /**
     * Provides usage information for the command associated with this calculation.
     * <p>
     * This message guides users on how to use the command to compute the factorial of a
     * specified integer.
     * </p>
     *
     * @return A string with the usage message.
     */
    @Override
    public String getUsageMessage() {
        return "  -fac [n] : Compute the factorial of [n]; valid range [" + MIN + ", " + MAX + "]";
    }
}
