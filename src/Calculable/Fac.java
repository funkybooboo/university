package Calculable;

import java.math.BigInteger;

public class Fac implements Calculable<BigInteger> {
    public static final long facMin = 0;
    public static final long facMax = Integer.MAX_VALUE;

    @Override
    public boolean isOutOfBounds(long n) {
        return n < facMin || n > facMax;
    }

    /**
     * Computes the factorial of a given non-negative integer.
     *
     * @param n The non-negative integer to compute the factorial of.
     * @return The factorial of the integer n.
     */
    @Override
    public BigInteger calculate(long n) {
        BigInteger result = BigInteger.ONE;
        for (int i = 2; i <= n; i++) {
            result = result.multiply(BigInteger.valueOf(i));
        }
        return result;
    }

    @Override
    public void printAnswer(long n, Object bigInteger) {
        System.out.println("Factorial of " + n + " is " + bigInteger);
    }

    @Override
    public void printRange() {
        System.out.println("Factorial valid range is [" + facMin + ", " + facMax + "]");
    }
}