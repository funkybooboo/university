public class Main {
    public static void main(String[] args) {
        System.out.println("\tRecursion");
        TeaCupSet.printChoices(10);
        System.out.println("-----------------------------");
        System.out.println("\tDynamic set 1");
        TeaCupSet teaCupSet1 = new TeaCupSet(1);
        teaCupSet1.printPriceMatrix();
        for (int i = 1; i <= 24; i++) {
            print(teaCupSet1, i);
        }
        System.out.println("-----------------------------");
        System.out.println("\tDynamic set 2");
        TeaCupSet teaCupSet2 = new TeaCupSet(2);
        teaCupSet2.printPriceMatrix();
        for (int i = 1; i <= 24; i++) {
            print(teaCupSet2, i);
        }
        System.out.println("-----------------------------");
    }

    private static void print(TeaCupSet teaCupSet, int i) {
        System.out.println("Best Sum for ("+ i +" teacups): "+"($"+ teaCupSet.getBestPrice(i)+")"+" sets: "+ teaCupSet.getCupSizes(i));
    }
}
