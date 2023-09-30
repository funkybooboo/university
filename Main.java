import java.util.*;

public class Main {
    public static void main(String[] args) {

        getNumberOfInversions(new ArrayList<>(List.of(4, 2, 9, 1, 7)));

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
        List<Integer> combined = merge1(lists.remove(0), lists.remove(1));
        while (!lists.isEmpty()) combined = merge1(lists.remove(0), combined);
        return combined;
    }
    public static List<Integer> merge1(List<Integer> A, List<Integer> B) {
        List<Integer> C = new ArrayList<>();
        while (!A.isEmpty() && !B.isEmpty()) {
            if (A.get(0) > B.get(0)) C.add(B.remove(0));
            else C.add(A.remove(0));
        }
        while (!A.isEmpty()) C.add(A.remove(0));
        while (!B.isEmpty()) C.add(B.remove(0));
        return C;
    }

    public static void getNumberOfInversions(List<Integer> list) {
        Pair pair = mergesort(list);
        System.out.println(pair.list);
        System.out.println(pair.count);
    }
    public static Pair mergesort(List<Integer> list) {
        if (list.isEmpty() || list.size() == 1) return new Pair(list, 0);
        int mid = (list.size()-1)/2+1;
        List<Integer> half1 = new ArrayList<>(list.subList(0, mid));
        List<Integer> half2 = new ArrayList<>(list.subList(mid, list.size()));
        Pair pair1 = mergesort(half1);
        Pair pair2 = mergesort(half2);
        return merge2(pair1.list, pair2.list, pair1.count + pair2.count);
    }
    public static Pair merge2(List<Integer> A, List<Integer> B, int count) {
        List<Integer> C = new ArrayList<>();
        while (!A.isEmpty() && !B.isEmpty()) {
            if (A.get(0) > B.get(0)) {
                count += A.size();
                C.add(B.remove(0));
            }
            else C.add(A.remove(0));
        }
        while (!A.isEmpty()) C.add(A.remove(0));
        while (!B.isEmpty()) C.add(B.remove(0));
        return new Pair(C, count);
    }
    public static class Pair {
        List<Integer> list;
        int count;
        public Pair(List<Integer> list, int count) {
            this.list = list;
            this.count = count;
        }
    }

}