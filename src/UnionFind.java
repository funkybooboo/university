import java.util.Arrays;
import java.util.Random;

public class UnionFind {

    // item : index | group : stored or -size if root
    // union-by-size

    int[] groups;

    public UnionFind(int[] elements) {
        groups = new int[elements.length];
        for (int i = 0; i < elements.length; i++) {
            groups[i] = -1;
        }
    }

    public void union(int indexA, int indexB) {
        int groupIndexA = find(indexA);
        int groupIndexB = find(indexB);
        if (groupIndexA == groupIndexB) return;
        if (groups[groupIndexA] <= groups[groupIndexB]) {
            groups[groupIndexB] = groupIndexA;
            groups[groupIndexA] -= 1;
        } else {
            groups[groupIndexA] = groupIndexB;
            groups[groupIndexB] -= 1;
        }
    }

    public int find(int indexA) {
        if (groups[indexA] < 0) {
            return indexA;
        }
        groups[indexA] = find(groups[indexA]);
        return groups[indexA];
    }

    public boolean isSameGroup(int indexA, int indexB) {
        if (groups[indexA] < 0 || groups[indexB] < 0) return false;
        int groupIndexA = find(indexA);
        int groupIndexB = find(indexB);
        return groupIndexA == groupIndexB;
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
        int size = 30;
        int[] data = new int[size];
        for (int i = 0; i < size; i++) {
            data[i] = i;
        }
        UnionFind unionFind = new UnionFind(data);
        System.out.println(unionFind);
        Random random = new Random();
        for (int i = 0; i < size/2; i++) {
            int a = random.nextInt(size);
            int b = random.nextInt(size);
            System.out.println("Union("+a+", "+b+")");
            unionFind.union(a, b);
            System.out.println(unionFind);
        }
    }

}
