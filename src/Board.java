import java.util.Random;

//functional handling of an instance of the board. Can be created using an id.
public class Board implements Comparable<Board> {
    private static final int SIZE = 3;
    private static final String SOLVED_ID = "123456780";
    private final int[][] board;  // Values of board
    private int blankRow;   // Row location of blank
    private int blankCol;   // Column location of blank
    private String steps = "";

    /**
     * Generate a new board
     */
    public Board() {
        board = new int[SIZE][SIZE];
        this.steps = "";
    }

    /**
     * Create board from string version
     *
     * @param id
     */
    public Board(String id, String steps) {
        board = new int[SIZE][SIZE];
        int c = 0;
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                if (id.charAt(c) == '0') {
                    blankRow = i;
                    blankCol = j;
                }
                //assigns each item of the string to the board as an int.
                board[i][j] = Integer.parseInt(id.substring(c, ++c));
            }
        }
        this.steps = steps;
    }

    public char getLast(){
        if (steps.equals("")) return '*';
        int last = steps.length();
        return steps.charAt(last-1);
    }

    public String getSteps() {
        return steps;
    }

    public int getNumSteps() {
        return steps.length();
    }

    public String stateAndStepsToString() {
        return "State " + getId() + " steps: " + steps;
    }

    public void setSteps(String steps) {
        this.steps += steps;
    }

    private int getPriority() {
        return getHammingDistance() + getNumSteps();
    }

    private int getHammingDistance() {
        String id = getId();
        int count = 0;
        for (int i = 0; i < id.length(); i++) {
            if (id.charAt(i) != SOLVED_ID.charAt(i)) count++;
        }
        return count;
    }

    /**
     * @param state String representation of the board
     * @return true if board state is the solution
     */
    Boolean isSolved(String state) {
        return state.equals(SOLVED_ID);
    }



    @Override
    /**
     * Convert matrix version of  board to ID
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (int[] i : board) {
            for (int j : i) {
                sb.append(j).append(" ");
            }
            sb.append("\n");
        }
        return sb.toString();
    }

    //Create a board by performing legal moves on a board
//jumbleCt indicates the number of moves to make
//if jumbleCt ==0, return the winning board

    /**
     * Create a solved board then make jumbleCt random moves
     *
     * @param jumbleCt number of random moves to make
     */
    public void makeBoard(int jumbleCt) {
        int val = 1;
        for (int i = 0; i < SIZE; i++)
            for (int j = 0; j < SIZE; j++)
                board[i][j] = val++;
        blankRow = SIZE - 1;
        blankCol = SIZE - 1;
        board[blankRow][blankCol] = 0;
        jumble(jumbleCt);
    }

    /**
     * create a board from a given set of values
     *
     * @param values values of board in order by rows
     */
    void makeBoard(int[] values) {
        int c = 0;
        for (int i = 0; i < SIZE; i++)
            for (int j = 0; j < SIZE; j++) {
                if (values[c] == 0) {
                    blankRow = i;
                    blankCol = j;
                }
                board[i][j] = values[c++];
            }
    }


    /**
     * Perform a slide up operation, if possible
     *
     * @return true if slide up was possible
     */
    boolean slideUp()  // If possible, slides a tile up into the blank position.  Returns success of operation.
    {
        if (blankRow == SIZE - 1) return false;
        board[blankRow][blankCol] = board[blankRow + 1][blankCol];
        board[blankRow + 1][blankCol] = 0;
        blankRow += 1;
        return true;
    }

    /**
     * Perform a slide Down operation, if possible
     *
     * @return true if slideDown was performed
     */
    boolean slideDown()  // If possible, slides a tile down into the blank position.  Returns success of operation.
    {
        if (blankRow == 0) return false;
        board[blankRow][blankCol] = board[blankRow - 1][blankCol];
        board[blankRow - 1][blankCol] = 0;
        blankRow -= 1;
        return true;
    }

    /**
     * Perform a slide Left, if possible
     *
     * @return true if slide Left was done
     */
    boolean slideLeft()  // If possible, slides a tile left into the blank position.  Returns success of operation.
    {
        if (blankCol == SIZE - 1) return false;
        board[blankRow][blankCol] = board[blankRow][blankCol + 1];
        board[blankRow][blankCol + 1] = 0;
        blankCol += 1;
        return true;
    }

    /**
     * Perform a slide Right, if possible
     *
     * @return true if slide Righrt was performed
     */
    boolean slideRight()  // If possible, slides a tile right into the blank position.  Returns success of operation.
    {
        if (blankCol == 0) return false;
        board[blankRow][blankCol] = board[blankRow][blankCol - 1];
        board[blankRow][blankCol - 1] = 0;
        blankCol -= 1;
        return true;
    }

    /**
     * Randomly apply ct moves to the board, making sure they are legal and don't undo the previous move
     *
     * @param moves
     */
    void jumble(int moves) {
        Random rand = new Random();
        String moveStr = "UDLR";  // Moves representing Up, Down, Left, Right
        char lastMove = ' ';
        char thisMove;
        for (int i = 0; i < moves; i++) {
            thisMove = moveStr.charAt(rand.nextInt(4));
            while (!makeMove(thisMove, lastMove)) {
                thisMove = moveStr.charAt(rand.nextInt(4));

            }
            lastMove = thisMove;
        }
    }

    /**
     * If move is legal (not  undoing previous move), make it
     * @param move     Move to attempt
     * @param lastmove Previously completed move
     * @return success of move
     */
    boolean makeMove(char move, char lastmove) {
        // System.out.println("makeMove " + move + lastmove + "\n");
        boolean moved = false;
        switch (move) {
            case 'R':
                if (lastmove != 'L') {
                    moved = slideRight();
                }
                break;
            case 'L':
                if (lastmove != 'R') {
                    moved = slideLeft();
                }
                break;
            case 'D':
                if (lastmove != 'U') {
                    moved = slideDown();
                }
                break;
            case 'U':
                if (lastmove != 'D') {
                    moved = slideUp();
                }
                break;
        }
        return moved;
    }

    /**
     * @return String version of board
     */
    public String getId() {
        StringBuilder id = new StringBuilder();
        for (int[] row : board) {
            for (int cell : row) {
                id.append(cell);
            }
        }
        return id.toString();
    }

    @Override
    public int compareTo(Board otherBoard) {
        if (this.getPriority() > otherBoard.getPriority()) {
            return 1;
        } else if (this.getPriority() < otherBoard.getPriority()) {
            return -1;
        }
        return 0;
    }
}
