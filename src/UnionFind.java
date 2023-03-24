import java.util.Arrays;

public class UnionFind {

    // item : index | group : stored or -size
    // union-by-size

    int[] groups;

    public UnionFind(int[] elements) {
        groups = new int[elements.length];
        for (int i = 0; i < elements.length; i++) {
            groups[i] = -1;
        }
    }

    public void union(int a, int b) {
        int groupA = find(a);
        int groupB = find(b);
        if (groups[groupA] <= groups[groupB]) {
            groups[groupB] = groupA;
            groups[groupA] -= 1;
        } else {
            groups[groupA] = groupB;
            groups[groupB] -= 1;
        }
    }

    public int find(int a) {
        if (groups[a] < 0) {
            return a;
        }
        groups[a] = find(groups[a]);
        return groups[a];
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(" [ item : group ] \n");
        for (int i = 0; i < groups.length; i++) {
            stringBuilder.append(" [ ").append(i).append(" : ").append(groups[i]).append(" ] ").append("\n");
        }
        return stringBuilder.toString();
    }

    public static void main(String[] args) {
        int[] data = new int[]{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        UnionFind unionFind = new UnionFind(data);
        unionFind.union(0, 1);
        unionFind.union(0, 2);
        unionFind.union(4, 5);
        unionFind.union(2, 4);
        unionFind.union(1, 7);
        unionFind.union(8, 6);
        unionFind.union(3, 6);
        System.out.println(unionFind);

    }

}
