import java.util.Scanner;
import java.util.*;

public class Game {
    private static final String SOLVED_ID = "123456780";
    Board theBoard;
    String originalBoardID;
    String boardName;

    /**
     *  Solve the provided board
     * @param label Name of board (for printing)
     * @param board Board to be solved
     */
    public void playGiven(String label, Board board) {
        theBoard = board;
        originalBoardID = board.getId();
        boardName = label;
        System.out.println("Board initial: " + boardName + " \n" + theBoard.toString());
        solve();

    }

    public void solve() {
        Queue<Board> movesToDo = new Queue<>();
        if (theBoard.slideRight()) {

        } else if (theBoard.slideLeft()) {

        } else if (theBoard.slideUp()) {

        } else if (theBoard.slideDown()) {

        } 


    }

    /**
     * Create a random board (which is solvable) by jumbling jumnbleCount times.
     * Solve
     * @param label Name of board (for printing)
     * @param jumbleCount number of random moves to make in creating a board
     */
    public void playRandom(String label, int jumbleCount) {
        theBoard = new Board();
        theBoard.makeBoard(jumbleCount);
        System.out.println(label + "\n" + theBoard);
        playGiven(label, theBoard);


    }
}
