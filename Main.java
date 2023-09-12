import java.util.Arrays;
import java.util.HashMap;

public class Main {
    public static void main(String[] args) {

        int K = 20;
        int[] items = {9, 3, 24, 14, 5, 8, 17, 4};

        System.out.println(Arrays.toString(solve(K, items)));
    }

    private static int[] solve(int K, int[] items) {
        HashMap<Integer, Integer> complements = new HashMap<>();
        // K complement
        // V item
        for (int item : items) {
            if (item > K) continue;
            if (item >= K / 2) {
                return new int[]{item};
            }
            Integer c = complements.get(item);
            if (c != null) {
                return new int[]{c, item};
            }
            int complement = K - item;
            complements.put(complement, item);
            for (int i = 1; i < complement; i++) {
                complements.put(i, item);
            }
        }

        return null;
    }

}
