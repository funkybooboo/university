import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Main {
    public static void main(String[] args) {

        int K = 20;
        //int[] items = {9, 3, 24, 14, 5, 8, 17, 4};
        //int[] items = {5, 1, 5, 1, 5, 1};
        int[] items = {1, 50, 7, 50, 50, 2};

        // n1 + n2 ... nn >= K/2 and n1 + n2 ... nn < K

        System.out.println(Arrays.toString(slowSolve(K, items)));
    }

    // O(n^2)
    public static int[] slowSolve(int K, int[] items) {
        for (int item1 : items) {
            if (item1 > K) continue;
            if (item1 >= K / 2) return new int[]{item1};

            List<Integer> solution = new ArrayList<>();
            int total = 0;

            solution.add(item1);
            total += item1;

            for (int j = 1; j < items.length; j++) {
                int item2 = items[j];
                if (item2 > K) continue;
                if (item2 >= K / 2) return new int[]{item2};

                solution.add(item2);
                total += item2;

                if (total > K) total -= item2;
                if (total >= K / 2) return solution.stream().mapToInt(Integer::intValue).toArray();
            }
        }
        return null;
    }

}