import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
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
        cellCount = (xSize * ySize);
        int totalCount = cellCount + 4;
        board = new String[totalCount];
        TOP = cellCount + 1;
        BOTTOM = cellCount + 2;
        LEFT = cellCount + 3;
        RIGHT = cellCount + 4;
        unionBlue = new UnionFind(totalCount);
        unionRed = new UnionFind(totalCount);
        setBoardBlank();
    }

    private void setBoardBlank() {
        for (int i = 0; i < cellCount; i++) {
            board[i] = getBlankPiece();
        }
        board[getIndex(TOP)] = getRedPiece();
        board[getIndex(BOTTOM)] = getRedPiece();
        board[getIndex(LEFT)] = getBluePiece();
        board[getIndex(RIGHT)] = getBluePiece();
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

    private List<Integer> getMoves(String filePath) {
        List<Integer> moves = new ArrayList<>();
        try {
            Scanner scanner = new Scanner(new File(filePath));
            while (scanner.hasNextLine()) {
                int item = scanner.nextInt();
                moves.add(item);
            }
        }
        catch (FileNotFoundException e) {
            System.out.println(e.toString());
        }
        return moves;
    }

    public void play(String filePath) {
        List<Integer> moves = getMoves(filePath);
        if (moves.isEmpty()) {
            return;
        }
        for (int i = 0; i < moves.size(); i++) {
            if (i % 2 == 0) {
                int blueMove = moves.get(i);
                if (move(blueMove, unionBlue, getBluePiece()) && isBlueWinner()) {
                    printWinner("Blue", i+1);
                    return;
                }
            }
            else {
                int redMove = moves.get(i);
                if (move(redMove, unionRed, getRedPiece()) && isRedWinner()) {
                    printWinner("Red", i+1);
                    return;
                }
            }
        }
        System.out.println("Ran out of moves and no winner");
    }

    private void printWinner(String title, int attempts) {
        System.out.println("--------> " + title +" has won after " + attempts + " attempted moves! Here is the final board.");
        printBoard();
    }

    private boolean isBlueWinner() {
        return unionBlue.isSameGroup(getIndex(LEFT), getIndex(RIGHT));
    }

    private boolean isRedWinner() {
        return unionRed.isSameGroup(getIndex(TOP), getIndex(BOTTOM));
    }

    private boolean isTaken(int item) {
        return !Objects.equals(board[getIndex(item)], getBlankPiece());
    }

    private boolean move(int item, UnionFind union, String myPiece) {
        if (isTaken(item)) return false;
        board[getIndex(item)] = myPiece;
        int[] neighbors = getNeighbors(item);
        for (int neighbor : neighbors) {
            int neighborIndex = getIndex(neighbor);
            if (board[neighborIndex].equals(getBlankPiece())) continue;
            if (board[neighborIndex].equals(myPiece)) union.union(neighborIndex, getIndex(item));
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
            String cell = board[i];
            stringBuilder.append(cell).append(" ");
            if ((i+1) % xSize == 0) {
                spaces.append(" ");
                stringBuilder.append("\n").append(spaces);
            }
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
