import java.util.Objects;
import java.util.Random;

public class Grid {

    private final int SIZE;
    private final String[][] GRID;

    private UnionFind unionFind;
    private final int topCell;
    private final int bottomCell;

    private final String BLUE = "\u001B[44m \u001B[0m";
    private final String WHITE = "\u001B[31m \u001B[0m";
    private final String BLACK = "\u001B[40m X \u001B[0m";
    private final String RED = "\u001B[41m \u001B[0m";
    private final String[] sym = {BLACK, WHITE, BLUE};

    public Grid(int size) {
        this.SIZE = size;
        topCell = SIZE*SIZE+1;
        bottomCell = SIZE*SIZE+2;
        this.GRID = new String[SIZE][SIZE];
        int[] IDs = new int[SIZE*SIZE+2];
        int count = 0;
        for (int x = 0; x < SIZE; x++) {
            for (int y = 0; y < SIZE; y++) {
                GRID[x][y] = BLACK;
                IDs[count] = getCellID(x, y);
                count += 1;
            }
        }
        IDs[SIZE*SIZE] = topCell;
        IDs[SIZE*SIZE+1] = bottomCell;
        this.unionFind = new UnionFind(IDs);
    }

    private int getCellID(int x, int y) {
        return SIZE*y+x;
    }

    private int[] getCellXY(int id) {
        int[] XYPair = new int[2];
        int x = id/SIZE;
        int y = id%SIZE;
        XYPair[0] = x;
        XYPair[1] = y;
        return XYPair;
    }

    public void printGrid() {
        for (int x = 0; x < SIZE; x++) {
            for (int y = 0; y < SIZE; y++) {
                System.out.print(GRID[x][y]);
            }
            System.out.println();
        }
    }

    private boolean isWaterPercolating() {
        return unionFind.find(topCell) == unionFind.find(bottomCell);
    }

    public boolean openRandomCell() {
        Random random = new Random();
        int x;
        int y;
        do {
            x = random.nextInt(SIZE);
            y = random.nextInt(SIZE);
        } while (!Objects.equals(GRID[x][y], BLACK));
        unionCellNeighbors(x, y);
        return isWaterPercolating();
    }

    public boolean openCell(int x, int y) {
        if (!Objects.equals(GRID[x][y], BLACK)) return false;
        unionCellNeighbors(x, y);
        return isWaterPercolating();
    }

    private void unionCellNeighbors(int x, int y) {
        GRID[x][y] = WHITE;
        if (x == 0) {
            GRID[x][y] = BLUE;
            unionFind.union(getCellID(x, y), topCell);
        }
        if (x == SIZE) {
            GRID[x][y] = BLUE;
            unionFind.union(getCellID(x, y), bottomCell);
        }
        if (!Objects.equals(GRID[x + 1][y], BLACK)) {
            unionCellNeighbor(x, y, x + 1, y);
        }
        if (!Objects.equals(GRID[x - 1][y], BLACK)) {
            unionCellNeighbor(x, y, x - 1, y);
        }
        if (!Objects.equals(GRID[x][y + 1], BLACK)) {
            unionCellNeighbor(x, y, x, y + 1);
        }
        if (!Objects.equals(GRID[x][y - 1], BLACK)) {
            unionCellNeighbor(x, y, x, y - 1);
        }
    }

    private void unionCellNeighbor(int thisCellX, int thisCellY, int neighborCellX, int neighborCellY) {
        if (Objects.equals(GRID[neighborCellX][neighborCellY], BLUE)) {
            GRID[thisCellX][thisCellY] = BLUE;
            unionFind.union(getCellID(thisCellX, thisCellY), getCellID(neighborCellX, neighborCellY));
        } else if (Objects.equals(GRID[neighborCellX][neighborCellY], WHITE) && Objects.equals(GRID[thisCellX][thisCellY], BLUE)) {
            GRID[neighborCellX][neighborCellY] = BLUE;
            unionFind.union(getCellID(thisCellX, thisCellY), getCellID(neighborCellX, neighborCellY));
        } else if (Objects.equals(GRID[neighborCellX][neighborCellY], WHITE) && Objects.equals(GRID[thisCellX][thisCellY], WHITE)) {
            unionFind.union(getCellID(thisCellX, thisCellY), getCellID(neighborCellX, neighborCellY));
        }
    }

}
