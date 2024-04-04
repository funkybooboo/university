import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class HexGame {

    private static final String ANSI_RESET = "\u001B[0m";
    private static final String ANSI_RED = "\u001B[31m";
    private static final String ANSI_BLUE = "\u001B[34m";
    private static final String ANSI_WHITE = "\u001B[37m";

    private final int xSize;
    private final int ySize;

    private final int cellCount;

    private final String[] board;

    private final UnionFind unionBlue;

    private final UnionFind unionRed;

    private final int TOP;
    private final int BOTTOM;
    private final int LEFT;
    private final int RIGHT;

    public HexGame() {
        this(11, 11);
    }

    public HexGame(int xSize, int ySize) {
        this.xSize = xSize;
        this.ySize = ySize;
        cellCount = (xSize * ySize) + 4;
        board = new String[cellCount];
        TOP = cellCount + 1;
        LEFT = cellCount + 2;
        RIGHT = cellCount + 3;
        BOTTOM = cellCount + 4;
        unionBlue = new UnionFind(cellCount);
        unionRed = new UnionFind(cellCount);
        setBoardBlank();
    }

    private void setBoardBlank() {
        for (int i = 0; i < cellCount; i++) {
            board[i] = getBlankPiece();
        }
    }

    private String getBlankPiece() {
        return ANSI_WHITE + "0" + ANSI_RESET;
    }

    private String getBluePiece() {
        return ANSI_BLUE + "B" + ANSI_RESET;
    }

    private String getRedPiece() {
        return ANSI_RED + "R" + ANSI_RESET;
    }

    private void getMoves(String filePath, List<Integer> blueMoves, List<Integer> redMoves) {
        try {
            Scanner scanner = new Scanner(new File(filePath));
            for (int i = 0; scanner.hasNextLine(); i++) {
                boolean isBlue = i % 2 == 0;
                int item = scanner.nextInt();
                if (isBlue) blueMoves.add(item);
                else redMoves.add(item);
            }
        }
        catch (FileNotFoundException e) {
            System.out.println(e.toString());
        }
    }

    public void play(String filePath) {
        List<Integer> blueMoves = new ArrayList<>();
        List<Integer> redMoves = new ArrayList<>();
        getMoves(filePath, blueMoves, redMoves);
        if (blueMoves.isEmpty() || redMoves.isEmpty()) {
            return;
        }
        for (int i = 0; i < blueMoves.size(); i++) {
            int blueMove = blueMoves.get(i);
            if (move(blueMove, unionBlue, unionRed)) {
                board[getIndex(blueMove)] = getBluePiece();
                if (isBlueWinner()) {
                    printWinner("Blue", i);
                    return;
                }
            }
            else {
                System.out.println("Blue invalid move: " + blueMove);
            }

            int redMove = redMoves.get(i);
            if (move(redMove, unionRed, unionBlue)) {
                board[getIndex(redMove)] = getRedPiece();
                if (isRedWinner()) {
                    printWinner("Red", i);
                    return;
                }
            }
            else {
                System.out.println("Red invalid move: " + redMove);
            }
        }
        System.out.println("Ran out of moves and no winner");
    }

    private void printWinner(String title, int attempts) {
        System.out.println("---------> " + title +" has won after " + attempts + " attempted moves! Here is the final board.");
        printBoard();
    }

    private boolean isBlueWinner() {
        return unionBlue.isSameGroup(RIGHT, LEFT);
    }

    private boolean isRedWinner() {
        return unionRed.isSameGroup(TOP, BOTTOM);
    }

    private boolean isTaken(int item) {
        boolean isBlueGroup = unionBlue.find(item) != -1;
        boolean isRedGroup = unionRed.find(item) != -1;
        return isBlueGroup || isRedGroup;
    }

    private boolean move(int item, UnionFind union, UnionFind otherUnion) {
        if (isTaken(item)) return false;
        int[] neighbors = getNeighbors(item);
        for (int neighbor : neighbors) {
            boolean isOtherGroup = otherUnion.find(neighbor) != -1;
            if (!isOtherGroup) union.union(neighbor, item);
        }
        return true;
    }

    private int getItem(int index) {
        return index + 1;
    }

    private int getIndex(int item) {
        return item - 1;
    }

    private boolean isRightEdge(int item) {
        return item % ySize == 0;
    }

    private boolean isLeftEdge(int item) {
        return (item - 1) % ySize == 0;
    }

    private boolean isTopEdge(int item) {
        return item <= xSize;
    }

    private boolean isBottomEdge(int item) {
        return item >= cellCount - xSize;
    }

    private boolean isEdge(int item) {
        return isLeftEdge(item) || isRightEdge(item) || isTopEdge(item) || isBottomEdge(item);
    }

    private int[] getNeighbors(int item) {
        int[] neighbors;
        if (isEdge(item)) {
            neighbors = getEdgeNeighbors(item);
        }
        else {
            neighbors = new int[]{
                item - 1,
                item + 1,
                item - 11,
                item - 10,
                item + 10,
                item + 11
            };
        }
        return neighbors;
    }

    private int[] getEdgeNeighbors(int item) {
        int[] neighbors = new int[6];
        if (isTopEdge(item)) {
            neighbors = new int[]{
                item - 1,
                item + 1,
                TOP,
                TOP,
                item + 10,
                item + 11
            };
        }
        else if (isBottomEdge(item)) {
            neighbors = new int[]{
                item - 1,
                item + 1,
                item - 11,
                item - 10,
                BOTTOM,
                BOTTOM
            };
        }
        else if (isLeftEdge(item)) {
            neighbors = new int[]{
                LEFT,
                item + 1,
                item - 11,
                item - 10,
                LEFT,
                item + 11
            };
        }
        else if (isRightEdge(item)) {
            neighbors = new int[]{
                item - 1,
                RIGHT,
                item - 11,
                RIGHT,
                item + 10,
                item + 11
            };
        }
        else if (isLeftEdge(item) && isTopEdge(item)) {
            neighbors = new int[]{
                LEFT,
                item + 1,
                TOP,
                TOP,
                LEFT,
                item + 11
            };
        }
        else if (isRightEdge(item) && isTopEdge(item)) {
            neighbors = new int[]{
                item - 1,
                RIGHT,
                TOP,
                TOP,
                item + 10,
                item + 11
            };
        }
        else if (isLeftEdge(item) && isBottomEdge(item)) {
            neighbors = new int[]{
                LEFT,
                item + 1,
                item - 11,
                item - 10,
                BOTTOM,
                BOTTOM
            };
        }
        else if (isRightEdge(item) && isBottomEdge(item)) {
            neighbors = new int[]{
                item - 1,
                RIGHT,
                item - 11,
                RIGHT,
                BOTTOM,
                BOTTOM
            };
        }
        return neighbors;
    }

    public void printBoard() {
        StringBuilder spaces = new StringBuilder();
        StringBuilder stringBuilder = new StringBuilder();
        for (int i = 0; i < cellCount; i++) {
            if ((i+1) % xSize == 0) {
                spaces.append(" ");
                stringBuilder.append("\n").append(spaces);
            }
            String cell = board[i];
            stringBuilder.append(cell).append(" ");

        }
        System.out.println(stringBuilder);
    }

    public static void main(String[] args) {
        HexGame hexGame = new HexGame();
        for (int i = 1; i < 3; i++) {
            String filePath = "data/moves"+i+".txt";
            System.out.println("Playing: " + filePath);
            hexGame.play(filePath);
            System.out.println();
        }
    }
}
