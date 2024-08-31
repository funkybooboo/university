import static Calculable.E.eMax;
import static Calculable.E.eMin;
import static Calculable.Fac.facMax;
import static Calculable.Fac.facMin;
import static Calculable.Fib.fibMax;
import static Calculable.Fib.fibMin;

public class Help {
    /**
     * Prints a message for an invalid command-line argument.
     *
     * @param flag The invalid argument.
     */
    public static void printUnknown(String flag) {
        System.out.println("Unknown command line argument: " + flag);
    }

    /**
     * Prints the help message for the program.
     */
    public static void printUsage() {
        System.out.println("--- Assign 1 Help ---");
        System.out.println("  -fib [n] : Compute the Fibonacci of [n]; valid range [" + fibMin + ", " + fibMax + "]");
        System.out.println("  -fac [n] : Compute the Factorial of [n]; valid range [" + facMin + ", " + facMax + "]");
        System.out.println("  -e   [n] : Compute the value of 'e' using [n] iterations; valid range [" + eMin + ", " + eMax + "]");
    }
}