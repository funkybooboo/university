package calculable;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * The {@code CalculableFactory} class provides static methods for getting instances
 * of {@code Calculable} implementations based on a specific flag.
 * It manages a static map that associates flags with their corresponding {@code Calculable} objects.
 * <p>
 * The factory initializes with a default set of {@code Calculable} implementations
 * for common calculations.
 * </p>
 *
 * @author Nate Stott
 */
public class CalculableFactory {

    // Static map to store flags and their corresponding Calculable implementations
    private static final Map<String, Calculable<?>> MAP = new HashMap<>();

    // Static initializer block to populate the map with default Calculable implementations
    static {
        MAP.put("-fib", new Fib());
        MAP.put("-fac", new Fac());
        MAP.put("-e", new E());
    }

    /**
     * Retrieves a {@code Calculable} instance associated with the specified flag.
     * <p>
     * This method looks up the flag in the internal map and returns the corresponding
     * {@code Calculable} object. If the flag is not found, the method returns {@code null}.
     * </p>
     *
     * @param flag the flag used to identify the desired {@code Calculable} implementation
     * @param <T> the type of the result produced by the {@code Calculable} implementation
     * @return the {@code Calculable} instance associated with the flag, or {@code null} if the flag is not found
     */
    @SuppressWarnings("unchecked")
    public static <T> Calculable<T> getCalculable(String flag) {
        Calculable<?> calculable = MAP.get(flag);
        if (calculable == null) {
            return null;
        }
        return (Calculable<T>) calculable;
    }

    /**
     * Retrieves all available {@code Calculable} instances.
     * <p>
     * This method returns a collection of all {@code Calculable} objects registered in
     * the factory. It provides a way to access all available implementations for listing
     * or help purposes.
     * </p>
     *
     * @return a collection of all {@code Calculable} instances
     */
    public static Collection<Calculable<?>> getCalculablesAll() {
        return MAP.values();
    }
}
