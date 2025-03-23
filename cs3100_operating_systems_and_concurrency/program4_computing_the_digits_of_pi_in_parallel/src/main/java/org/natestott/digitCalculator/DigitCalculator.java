package org.natestott.digitCalculator;

/**
 * The DigitCalculator interface defines a contract for classes
 * that compute an arbitrary digit of any number.
 *
 * <p>Implementing classes must provide the logic for calculating
 * the digit at a given position in a numerical representation,
 * such as digits of Pi.</p>
 *
 * @author Nate Stott
 */
public interface DigitCalculator {

    /**
     * Computes the digit at the specified position.
     *
     * @param n The position in the numerical representation for which
     *           the digit is to be computed.
     * @return The digit at the specified position.
     */
    int getDigit(long n);
}
