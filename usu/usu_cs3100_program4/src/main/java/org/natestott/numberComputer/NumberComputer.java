package org.natestott.numberComputer;

/**
 * The NumberComputer interface defines the contract for classes that compute
 * numerical values, specifically focusing on the calculation and output of
 * numerical representations, such as digits of Pi.
 *
 * <p>Implementing classes should provide the logic for computing numbers
 * and printing the results.</p>
 *
 * @author Nate Stott
 */
public interface NumberComputer {

    /**
     * Computes a default number of digits and prints the result.
     *
     * <p>This method is typically intended to compute a standard
     * number of digits (e.g., 1000) and print the outcome.</p>
     */
    void computeNumberAndPrint();

    /**
     * Computes the specified number of digits and prints the result.
     *
     * @param num_digits The number of digits to compute and print.
     *                   Implementing classes should handle this parameter
     *                   to compute the specified number of digits accurately.
     */
    void computeNumberAndPrint(final int num_digits);
}
