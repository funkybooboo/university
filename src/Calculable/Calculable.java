package Calculable;

public interface Calculable<T> {
    boolean isOutOfBounds(long n);

    T calculate(long n);

    void printAnswer(long n, Object t);

    void printRange();
}
