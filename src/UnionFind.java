public class UnionFind {

    // item : index | group : stored or -size
    // union-by-size

    int[] groups;

    public UnionFind(int[] elements) {
        groups = new int[elements.length];
        for (int i = 0; i < elements.length; i++) {
            groups[i] = -1*i;
        }
    }

    public void union(int a, int b) {


    }

    public int find(int a) {
        if (groups[a] < 0) {
            return a;
        }
        groups[a] = find(groups[a]);
        return groups[a];
    }

}
