import java.util.*;

public class Main {
    public static void main(String[] args) {

        List<Integer> list1 = new ArrayList<>(List.of(3, 12, 19, 25, 36));
        List<Integer> list2 = new ArrayList<>(List.of(34, 89));
        List<Integer> list3 = new ArrayList<>(List.of(17, 26, 87));
        List<Integer> list4 = new ArrayList<>(List.of(28));
        List<Integer> list5 = new ArrayList<>(List.of(2, 10, 21, 29, 55, 59, 61));

        List<List<Integer>> lists = new ArrayList<>(List.of(list1, list2, list3, list4, list5));
        List<Integer> list = mergeLists(lists);
        System.out.println(list);

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

    public static List<Integer> mergeLists(List<List<Integer>> lists) {
        if (lists.isEmpty()) return new ArrayList<>();
        if (lists.size() == 1) return lists.remove(0);
        List<Integer> combined = merge(lists.remove(0), lists.remove(1));
        while (!lists.isEmpty()) combined = merge(lists.remove(0), combined);
        return combined;
    }
    public static List<Integer> merge(List<Integer> A, List<Integer> B) {
        List<Integer> C = new ArrayList<>();
        while (!A.isEmpty() && !B.isEmpty()) {
            if (A.get(0) > B.get(0)) C.add(B.remove(0));
            else C.add(A.remove(0));
        }
        while (!A.isEmpty()) C.add(A.remove(0));
        while (!B.isEmpty()) C.add(B.remove(0));
        return C;
    }

}