package Calculable;

public interface Calculable<T> {
    boolean bounds(long n);

    T calculate(long n);

    void printAnswer(long n, Object t);

    void printRange();
}
