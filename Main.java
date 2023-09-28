import java.util.*;

public class Main {
    public static void main(String[] args) {
        double x = 11;
        int n = 10;
        List<Double> list1 = List.of();
        List<Double> list2 = List.of();
        findSum(x, n, list1, list2);
    }

    public static void findSum(double x, int n, List<Double> list1, List<Double> list2) {
        if (list1.isEmpty() || list2.isEmpty()) {
            System.out.println("None");
            return;
        }
        Map<Double, Integer> map1 = new HashMap<>();
        Map<Double, Integer> map2 = new HashMap<>();
        // K V -> complement index_of_num
        for (int i = 0; i < n; i++) {
            double a = list1.get(i);
            double b = list2.get(i);
            map1.put(x - b, i);
            map2.put(x - a, i);
            if (map1.containsKey(a)) {
                System.out.println(a + " " + list2.get(map1.get(a)));
                return;
            }
            if (map2.containsKey(b)) {
                System.out.println(list1.get(map2.get(b)) + " " + b);
                return;
            }
        }
        System.out.println("None");
    }
}