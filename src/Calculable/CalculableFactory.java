package Calculable;

import java.util.HashMap;
import java.util.Map;

public class CalculableFactory {

    private final Map<String, Calculable<?>> map = new HashMap<>();

    public CalculableFactory() {
        map.put("-fib", new Fib());
        map.put("-fac", new Fac());
        map.put("-e", new E());
    }

    @SuppressWarnings("unchecked")
    public <T> Calculable<T> getCalculable(String flag) {
        Calculable<?> calculable = map.get(flag);
        if (calculable == null) {
            return null;
        }
        return (Calculable<T>) calculable;
    }
}
