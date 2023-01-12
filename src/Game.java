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

    // *BONUS QUESTIONS*
    // Making the code faster
    // ----------------------
    // 1.
    // I dont know if this counts as bonus points for making the program faster, but
    // I noticed that in the Board.getId() function you where using String concatenation.
    // But it would be faster to use a StringBuilder Object instead so that when you do += on strings a whole new string doesn't have to be created.
    // Granted this change wouldn't make a huge difference to program speed, but it would help.
    // 2.
    // I think putting the classes Board and State together would make the program faster.
    // If you combine the Board and State classes than the program wouldn't have to keep track of two objects that are very identical.
    // 3.
    // Instead of using a List to keep track of all the previously visited states you could use a HashSet sense a HashSet would be faster in checking if a State is in the set.

    // How to see if a board is unsolvable?
    // ------------------------------------
    // In my experimentation on the online Slider Game I realized that in trying to get the board 123456870 you always end up solving the board on accident even when not trying to win.
    // So my theory is that a board is unsolvable if starting board looks like the winning board (123456780) but has two blocks swapped (horizontally or vertically).
    // Examples: 132456780, 123465780, 123756480, 423156780

    public void solve() {
        Queue<State> movesToDo = new Queue<>();
        State currentState = new State(theBoard.getId(), "");
        Board currentBoard = new Board(theBoard.getId());
        List<String> visitedStates = new ArrayList<>();
        int queueAdded = 0;
        int queueRemoved = 0;
        boolean solved = true;
        visitedStates.add(currentState.getId());
        while (!currentBoard.isSolved(currentState.getId())) {
            char lastMove = currentState.getLast();
            if (lastMove != 'L') {
                if (currentBoard.slideRight()) {
                    queueAdded = addToQueue(movesToDo, currentState, currentBoard, visitedStates, queueAdded, "R");
                    currentBoard.slideLeft();
                }
            }
            if (lastMove  != 'R') {
                if (currentBoard.slideLeft()) {
                    queueAdded = addToQueue(movesToDo, currentState, currentBoard, visitedStates, queueAdded, "L");
                    currentBoard.slideRight();
                }
            }
            if (lastMove != 'D') {
                if (currentBoard.slideUp()) {
                    queueAdded = addToQueue(movesToDo, currentState, currentBoard, visitedStates, queueAdded, "U");
                    currentBoard.slideDown();
                }
            }
            if (lastMove != 'U') {
                if (currentBoard.slideDown()) {
                    queueAdded = addToQueue(movesToDo, currentState, currentBoard, visitedStates, queueAdded, "D");
                    currentBoard.slideDown();
                }
            }
            if (visitedStates.size() > 1_000_000){
                System.out.println("There is no solution");
                solved = false;
                break;
            } else if (movesToDo.getSIZE() > 0) {
                currentState = movesToDo.removeFront();
                currentBoard = new Board(currentState.getId());
                queueRemoved++;
            } else {
                System.out.println("Unable to determine if there is a solution");
                solved = false;
                break;
            }
        }
        if (solved) {
            System.out.println("Moves Required: " + currentState.getSteps() + "(" + currentState.getNumSteps() + ")");
            System.out.println("Queue added=" + queueAdded + " Removed=" + queueRemoved);
        }
        System.out.println();
    }

    private int addToQueue(Queue<State> movesToDo, State currentState, Board currentBoard, List<String> visitedStates, int queueAdded, String letter) {
        State movedState = new State(currentBoard.getId(), currentState.getSteps() + letter);
        visitedStates.add(movedState.getId());
        movesToDo.add(movedState);
        return ++queueAdded;
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
