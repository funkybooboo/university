import java.util.Objects;
import java.util.Random;

public class WaterPercolationGrid {

    private final int SIZE;
    private final String[][] GRID;
    private final UnionFind unionFind;
    private final int topCellID;
    private final int bottomCellID;

    public WaterPercolationGrid(int size) {
        this.SIZE = size;
        topCellID = SIZE*SIZE;
        bottomCellID = (SIZE*SIZE)+1;
        this.GRID = new String[SIZE][SIZE];
        int[] IDs = new int[(SIZE*SIZE)+2];
        int count = 0;
        for (int x = 0; x < SIZE; x++) {
            for (int y = 0; y < SIZE; y++) {
                GRID[x][y] = "black";
                IDs[count] = getCellID(x, y);
                count += 1;
            }
        }
        IDs[SIZE*SIZE] = topCellID;
        IDs[SIZE*SIZE+1] = bottomCellID;
        this.unionFind = new UnionFind(IDs);
    }

    private int getCellID(int x, int y) {
        return (SIZE*y)+x;
    }

    private int[] getCellXY(int id) {
        int[] XYPair = new int[2];
        int x = id/SIZE;
        int y = id%SIZE;
        XYPair[0] = x;
        XYPair[1] = y;
        return XYPair;
    }

    // ANSI Colors Java
    private String getStringColor(String color) {
        if (Objects.equals(color, "black")) {
            return "\u001B[40m ` \u001B[0m";
        } else if (Objects.equals(color, "white")) {
            return "\u001B[31m ` \u001B[0m";
        } else if (Objects.equals(color, "blue")) {
            return "\u001B[44m ` \u001B[0m";
        } else {
            return "\u001B[41m ` \u001B[0m";
        }
    }

    public void printGrid(int numberOfCellsOpen) {
        for (int x = 0; x < SIZE; x++) {
            for (int y = 0; y < SIZE; y++) {
                System.out.print(getStringColor(GRID[x][y]));
            }
            System.out.println();
        }
        System.out.println(numberOfCellsOpen+" open cells\n");
    }

    private boolean isWaterPercolating() {
        return unionFind.isSameGroup(topCellID, bottomCellID);
    }

    private boolean openCell(int x, int y) {
        if (x < 0 || y < 0 || x > SIZE-1 || y > SIZE-1) return false;
        if (!Objects.equals(GRID[x][y], "black")) return false;
        GRID[x][y] = "white";
        unionCellNeighbors(x, y);
        if (isWaterPercolating()) {
            GRID[x][y] = "red";
            redify(x, y);
            return true;
        } else {
            return false;
        }
    }

    public boolean openRandomCell() {
        Random random = new Random();
        int x;
        int y;
        do {
            x = random.nextInt(SIZE);
            y = random.nextInt(SIZE);
        } while (!Objects.equals(GRID[x][y], "black"));
        GRID[x][y] = "white";
        unionCellNeighbors(x, y);
        if (isWaterPercolating()) {
            GRID[x][y] = "red";
            redify(x, y);
            return true;
        } else {
            return false;
        }
    }

    private void redify(int x, int y) {
        if (x < 0 || y < 0 || x > SIZE-1 || y > SIZE-1) return;
        if (Objects.equals(GRID[x][y], "black") || Objects.equals(GRID[x][y], "white")) return;
        if (x != SIZE-1 && Objects.equals(GRID[x + 1][y], "blue")) {
            GRID[x + 1][y] = "red";
            redify(x + 1, y);
        }
        if (x != 0 && Objects.equals(GRID[x - 1][y], "blue")) {
            GRID[x - 1][y] = "red";
            redify(x - 1, y);
        }
        if (y != SIZE-1 && Objects.equals(GRID[x][y + 1], "blue")) {
            GRID[x][y + 1] = "red";
            redify(x, y + 1);
        }
        if (y != 0 && Objects.equals(GRID[x][y - 1], "blue")) {
            GRID[x][y - 1] = "red";
            redify(x, y - 1);
        }
    }

    private void unionCellNeighbors(int x, int y) {
        if (x < 0 || y < 0 || x > SIZE-1 || y > SIZE-1) return;
        if (Objects.equals(GRID[x][y], "black")) return;
        if (x == 0) {
            GRID[x][y] = "blue";
            unionFind.union(getCellID(x, y), topCellID);
        }
        if (x == SIZE-1) {
            unionFind.union(getCellID(x, y), bottomCellID);
        }
        if (x != SIZE-1 && !Objects.equals(GRID[x + 1][y], "black")) {
            unionFind.union(getCellID(x, y), getCellID(x + 1, y));
            colorNeighbor(x, y, x + 1, y);
        }
        if (x != 0 && !Objects.equals(GRID[x - 1][y], "black")) {
            unionFind.union(getCellID(x, y), getCellID(x - 1, y));
            colorNeighbor(x, y, x - 1, y);
        }
        if (y != SIZE-1 && !Objects.equals(GRID[x][y + 1], "black")) {
            unionFind.union(getCellID(x, y), getCellID(x, y + 1));
            colorNeighbor(x, y, x, y + 1);
        }
        if (y != 0 && !Objects.equals(GRID[x][y - 1], "black")) {
            unionFind.union(getCellID(x, y), getCellID(x, y - 1));
            colorNeighbor(x, y, x, y - 1);
        }
    }

    private void colorNeighbor(int thisCellX, int thisCellY, int neighborCellX, int neighborCellY) {
        if (Objects.equals(GRID[neighborCellX][neighborCellY], "blue") && Objects.equals(GRID[thisCellX][thisCellY], "white")) {
            GRID[thisCellX][thisCellY] = "blue";
            int neighborCellID = getCellID(neighborCellX, neighborCellY);
            if (thisCellX != SIZE-1 && getCellID(thisCellX + 1, thisCellY) != neighborCellID) {
                unionCellNeighbors(thisCellX + 1, thisCellY);
            }
            if (thisCellX != 0 && getCellID(thisCellX - 1, thisCellY) != neighborCellID) {
                unionCellNeighbors(thisCellX - 1, thisCellY);
            }
            if (thisCellY != SIZE-1 && getCellID(thisCellX, thisCellY + 1) != neighborCellID) {
                unionCellNeighbors(thisCellX, thisCellY + 1);
            }
            if (thisCellY != 0 && getCellID(thisCellX, thisCellY - 1) != neighborCellID) {
                unionCellNeighbors(thisCellX, thisCellY - 1);
            }
        } else if (Objects.equals(GRID[neighborCellX][neighborCellY], "white") && Objects.equals(GRID[thisCellX][thisCellY], "blue")) {
            GRID[neighborCellX][neighborCellY] = "blue";
            int thisCellID = getCellID(thisCellX, thisCellY);
            if (neighborCellX != SIZE-1 && getCellID(neighborCellX + 1, neighborCellY) != thisCellID) {
                unionCellNeighbors(neighborCellX + 1, neighborCellY);
            }
            if (neighborCellX != 0 && getCellID(neighborCellX - 1, neighborCellY) != thisCellID) {
                unionCellNeighbors(neighborCellX - 1, neighborCellY);
            }
            if (neighborCellY != SIZE-1 && getCellID(neighborCellX, neighborCellY + 1) != thisCellID) {
                unionCellNeighbors(neighborCellX, neighborCellY + 1);
            }
            if (neighborCellY != 0 && getCellID(neighborCellX, neighborCellY - 1) != thisCellID) {
                unionCellNeighbors(neighborCellX, neighborCellY - 1);
            }
        }
    }

    public int runSimulation() {
        int numberOfCellsOpened = 0;
        while (!openRandomCell()) {
            numberOfCellsOpened += 1;
            if (numberOfCellsOpened % 50 == 0) {
                printGrid(numberOfCellsOpened);
            }
        }
        printGrid(numberOfCellsOpened);
        return numberOfCellsOpened;
    }

    public static void main(String[] args) {
        int size = 20;
        int numberOfTrials = 5;
        int count = numberOfTrials;
        double[] stats = new double[count];
        int numBlocks = size*size;
        while (count > 0) {
            WaterPercolationGrid waterPercolationGrid = new WaterPercolationGrid(size);
            int numberOfCellsOpened = waterPercolationGrid.runSimulation();
            double percentage = 100*((double) numberOfCellsOpened / (double) numBlocks);
            stats[count-1] = percentage;
            System.out.println("Grid size: "+size+"x"+size);
            System.out.println("Number of cells: "+numBlocks);
            System.out.printf("Percentage of grid open: %.2f", percentage);
            System.out.println("%\n");
            count--;
        }
        double sum = 0;
        for (double stat : stats) {
            sum += stat;
        }
        double overallPercentage = sum / (double) numberOfTrials;
        System.out.printf("\nAverage percentage after "+numberOfTrials+" simulations: %.2f", overallPercentage);
        System.out.println("%");
    }

}
