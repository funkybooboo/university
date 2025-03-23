package calculable;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * The {@code E} class implements the {@code Calculable} interface to compute
 * Euler's number 'e' using a specified number of iterations.
 * The value is calculated to a precision of 16 decimal places.
 * <p>
 * This class uses the series expansion of 'e' for the calculation:
 * <pre>
 *     e = 1 + 1/1! + 1/2! + 1/3! + ... + 1/n!
 * </pre>
 * </p>
 *
 * @author Nate Stott
 */
public class E implements Calculable<BigDecimal> {

    // Constants for the valid range of iterations
    public static final long MIN = 1;
    public static final long MAX = Integer.MAX_VALUE;

    /**
     * Checks if the given number of iterations is out of bounds.
     * <p>
     * The valid range for the number of iterations is from {@code MIN} to {@code MAX}.
     * </p>
     *
     * @param n the number of iterations to check
     * @return {@code true} if the number of iterations is out of bounds, {@code false} otherwise
     */
    @Override
    public boolean isOutOfBounds(long n) {
        return n < MIN || n > MAX;
    }

    /**
     * Computes the value of Euler's number 'e' using the specified number of iterations.
     * <p>
     * The calculation is performed using the series expansion of 'e' with a precision
     * of 16 decimal places.
     * </p>
     *
     * @param n The number of iterations to use in the calculation (must be >= 1).
     * @return The value of 'e' calculated to 16 decimal places.
     */
    @Override
    public BigDecimal calculate(long n) {
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

    /**
     * Generates a message displaying the calculated value of 'e'.
     * <p>
     * The message includes the number of iterations used and the computed value of 'e'.
     * </p>
     *
     * @param n The number of iterations used in the calculation.
     * @param bigDecimal The calculated value of 'e'.
     * @return A formatted string with the result message.
     */
    @Override
    public String getResultMessage(long n, Object bigDecimal) {
        return "Value of e using " + n + " iterations is " + bigDecimal;
    }

    /**
     * Provides a message describing the valid range of iterations for the calculation.
     * <p>
     * This message informs users about the acceptable range for the number of iterations.
     * </p>
     *
     * @return A string describing the range of valid values.
     */
    @Override
    public String getRangeMessage() {
        return "Calculable.E valid range is [" + MIN + ", " + MAX + "]";
    }

    /**
     * Provides usage information for the command associated with this calculation.
     * <p>
     * This message guides users on how to use the command to compute 'e' with a specified
     * number of iterations.
     * </p>
     *
     * @return A string with the usage message.
     */
    @Override
    public String getUsageMessage() {
        return "  -e   [n] : Compute the value of 'e' using [n] iterations; valid range [" + MIN + ", " + MAX + "]";
    }
}
