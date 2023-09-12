import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

public class Main {
    public static void main(String[] args) {

        int K = 20;
        //int[] items = {9, 3, 24, 14, 5, 8, 17, 4};
        int[] items = {5, 1, 5, 1, 5, 1};

        // n1 + n2 ... nn >= K/2 and n1 + n2 ... nn < K

        System.out.println(Arrays.toString(slowSolve(K, items)));
    }

    // O(n^2)
    public static int[] slowSolve(int K, int[] items) {
        for (int i = 0; i < items.length; i++) {
            List<Integer> solution = new ArrayList<>();
            int item1 = items[i];
            if (item1 > K) continue;
            if (item1 >= K/2) return new int[]{item1};
            solution.add(item1);
            for (int j = 1; j < items.length; j++) {
                int item2 = items[j];
                if (item2 > K) continue;
                if (item2 >= K/2) return new int[]{item2};
                solution.add(item2);

                int total = 0;
                for (int n : solution) {
                    total += n;
                }
                if (total > K) continue;
                if (total >= K/2) return solution.stream().mapToInt(Integer::intValue).toArray();
            }
        }
        return null;
    }

}