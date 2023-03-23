public class MonteCarloSimulation {

    int SIZE;
    int[][] grid = new int[SIZE][SIZE];
    String BLUE = "\u001B[44m \u001B[0m";
    String WHITE = "\u001B[31m \u001B[0m";
    String BLACK = "\u001B[40m X \u001B[0m";
    String RED = "\u001B[41m \u001B[0m";
    String[] sym = {BLACK, WHITE, BLUE};

    public MonteCarloSimulation(int size) {
        this.SIZE = size;
    }

    public void printRandomGrid() {
        for (int x = 0; x < SIZE; x++) {
            for (int y = 0; y < SIZE; y++) {
                grid[x][y] = (int)(Math.random()*sym.length);
                System.out.print(sym[grid[x][y]]);
            }
            System.out.println();
        }

    }

    public int getCellID(int x, int y) {
        return SIZE*y+x;
    }

    public int[] getCellXY(int id) {
        int[] XYPair = new int[2];
        int x = id/SIZE;
        int y = id%SIZE;
        XYPair[0] = x;
        XYPair[1] = y;
        return XYPair;
    }

    public static void main(String[] args) {
        MonteCarloSimulation monteCarloSimulation = new MonteCarloSimulation(20);
        monteCarloSimulation.printRandomGrid();

    }



}
