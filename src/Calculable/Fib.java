package Calculable;

public class Fib implements Calculable<Integer> {
    public static final long fibMin = 0;
    public static final long fibMax = 40;

    @Override
    public boolean bounds(long n) {
        return n < fibMin || n > fibMax;
    }

    /**
     * Computes the Fibonacci number for a given non-negative integer.
     *
     * @param n The index of the Fibonacci sequence (non-negative).
     * @return The Fibonacci number at index n.
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

    @Override
    public void printAnswer(long n, Object integer) {
        System.out.println("Fibonacci of " + n + " is " + integer);

    }

    @Override
    public void printRange() {
        System.out.println("Fibonacci valid range is [" + fibMin + ", " + fibMax + "]");
    }
}