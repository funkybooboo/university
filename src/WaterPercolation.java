
public class WaterPercolation {

    Grid grid;

    public WaterPercolation(int size) {
        grid = new Grid(size);
    }

    public int runSimulation() {
        int numberOfCellsOpened = 0;
        System.out.println("Start board");
        grid.printGrid();
        while (grid.openRandomCell()) {
            System.out.println("\n");
            System.out.println("run "+numberOfCellsOpened+1);
            grid.printGrid();
            numberOfCellsOpened++;
        }
        System.out.println("\n");
        System.out.println("Final board");
        grid.printGrid();
        return numberOfCellsOpened;
    }

    public static void main(String[] args) {
        WaterPercolation waterPercolation = new WaterPercolation(20);
        int numberOfCellsOpened = waterPercolation.runSimulation();


    }

}
