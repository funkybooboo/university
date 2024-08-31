package Calculable;

import java.math.BigDecimal;
import java.math.RoundingMode;

public class E implements Calculable<BigDecimal> {
    public static final long eMin = 1;
    public static final long eMax = Integer.MAX_VALUE;

    @Override
    public boolean bounds(long n) {
        return n < eMin || n > eMax;
    }

    /**
     * Computes the value of Euler's number 'e' using a specified number of iterations.
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

    @Override
    public void printAnswer(long n, Object bigDecimal) {
        System.out.println("Value of e using " + n + " iterations is " + bigDecimal);

    }

    @Override
    public void printRange() {
        System.out.println("Calculable.E valid range is [" + eMin + ", " + eMax + "]");
    }
}