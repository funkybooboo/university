package calculable;

/**
 * The {@code Calculable} interface defines methods for performing calculations
 * and managing results.
 * Implementations of this interface must specify how to determine if a value is out of bounds,
 * perform a calculation, and generate messages for results and valid value ranges.
 *
 * @param <T> the type of the result produced by the calculation
 *
 * @author Nate Stott
 */
public interface Calculable<T> {

    /**
     * Determines if the given value is out of the acceptable range for the calculation.
     * <p>
     * Implementations should define what constitutes "out of bounds" for their specific
     * calculation logic.
     * </p>
     *
     * @param n the value to check
     * @return {@code true} if the value is out of bounds, {@code false} otherwise
     */
    boolean isOutOfBounds(long n);

    /**
     * Performs a calculation using the given value.
     * <p>
     * Implementations should define how to compute the result based on the provided
     * value {@code n}.
     * </p>
     *
     * @param n the value to use in the calculation
     * @return the result of the calculation
     */
    T calculate(long n);

    /**
     * Generates a message displaying the result of the calculation.
     * <p>
     * The message should incorporate the provided value {@code n} and the result
     * {@code t} to present a meaningful output.
     * </p>
     *
     * @param n the value used in the calculation
     * @param t the result of the calculation
     * @return a formatted string with the result message
     */
    String getResultMessage(long n, Object t);

    /**
     * Provides a message that describes the valid range of values for the calculation.
     * <p>
     * Implementations should return a message that informs users about the acceptable
     * value range for their specific calculations.
     * </p>
     *
     * @return a string describing the range of valid values
     */
    String getRangeMessage();

    /**
     * Provides usage information for the command associated with this calculation.
     * <p>
     * This message should guide users on how to correctly use the command.
     * </p>
     *
     * @return a string with the usage message
     */
    String getUsageMessage();
}
