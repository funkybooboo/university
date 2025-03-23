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
        if (indexA < 0 || indexA > groups.length-1) return;
        if (indexB < 0 || indexB > groups.length-1) return;
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

    public Integer find(int indexA) {
        if (indexA < 0 || indexA > groups.length-1) return null;
        if (groups[indexA] < 0) {
            return indexA;
        }
        groups[indexA] = find(groups[indexA]);
        return groups[indexA];
    }

    public boolean isSameGroup(int indexA, int indexB) {
        if (indexA < 0 || indexA > groups.length-1) return false;
        if (indexB < 0 || indexB > groups.length-1) return false;
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
        test();
    }

    private static void test() {

        System.out.println("Union Two Small Groups Test");
        int[] data = new int[]{0, 1, 2, 3, 4, 5};
        UnionFind unionFind = new UnionFind(data);
        System.out.println(unionFind);
        unionFind.union(0, 1);
        System.out.println("Union("+0+", "+1+")");
        System.out.println(unionFind);
        unionFind.union(1, 2);
        System.out.println("Union("+1+", "+2+")");
        System.out.println(unionFind);
        unionFind.union(3, 4);
        System.out.println("Union("+3+", "+4+")");
        System.out.println(unionFind);
        unionFind.union(4, 5);
        System.out.println("Union("+4+", "+5+")");
        System.out.println(unionFind);
        System.out.println("Small groups made");
        System.out.println(unionFind);
        unionFind.union(2, 5);
        System.out.println("Union("+2+", "+5+")");
        System.out.println("Union the small groups to make one larger group");
        System.out.println(unionFind);
        System.out.println("---------------\n");

        System.out.println("Union two items together that are already in the same group");
        System.out.println(unionFind);
        unionFind.union(2, 5);
        System.out.println("Union("+2+", "+5+")");
        System.out.println(unionFind);
        System.out.println("---------------\n");

        System.out.println("Show path compression");
        System.out.println(unionFind);
        unionFind.find(4);
        System.out.println("Find("+4+")");
        System.out.println(unionFind);
        System.out.println("---------------\n");

        System.out.println("Union and find items out of bounds");
        unionFind = new UnionFind(data);
        System.out.println(unionFind);
        unionFind.find(-1);
        System.out.println("Find("+-1+")");
        System.out.println(unionFind);
        unionFind.find(6);
        System.out.println("Find("+6+")");
        System.out.println(unionFind);
        unionFind.union(-1, 4);
        System.out.println("Union("+-1+", "+4+")");
        System.out.println(unionFind);
        unionFind.union(1, 6);
        System.out.println("Union("+1+", "+6+")");
        System.out.println(unionFind);
        System.out.println("---------------\n");

        System.out.println("Union the same item");
        System.out.println(unionFind);
        unionFind.union(4, 4);
        System.out.println("Union("+4+", "+4+")");
        System.out.println(unionFind);
        System.out.println("---------------\n");

        System.out.println("Random Union Test");
        int size = 15;
        data = new int[size];
        for (int i = 0; i < size; i++) {
            data[i] = i;
        }
        unionFind = new UnionFind(data);
        System.out.println(unionFind);
        Random random = new Random();
        for (int i = 0; i < size; i++) {
            int a = random.nextInt(size);
            int b = random.nextInt(size);
            System.out.println("Union("+a+", "+b+")");
            unionFind.union(a, b);
            System.out.println(unionFind);
        }
        System.out.println("---------------\n");
    }


}
