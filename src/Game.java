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
    
    private void solve() {
        AVLTree<Board> movesToDo = new AVLTree<>();
        Board currentBoard = new Board(theBoard.getId());
        List<String> visitedStates = new ArrayList<>();
        int queueAdded = 0;
        int queueRemoved = 0;
        boolean solved = true;
        int numOfIterations = 0;
        visitedStates.add(currentBoard.getId());
        System.out.println("[" + currentBoard.stateAndStepsToString() + "]");
        while (!currentBoard.isSolved(currentBoard.getId())) {
            char lastMove = currentBoard.getLast();
            if (lastMove != 'L') {
                if (currentBoard.slideRight()) {
                    queueAdded = addToQueue(movesToDo, currentBoard, visitedStates, queueAdded, "R");
                    currentBoard.slideLeft();
                }
            }
            if (lastMove  != 'R') {
                if (currentBoard.slideLeft()) {
                    queueAdded = addToQueue(movesToDo, currentBoard, visitedStates, queueAdded, "L");
                    currentBoard.slideRight();
                }
            }
            if (lastMove != 'D') {
                if (currentBoard.slideUp()) {
                    queueAdded = addToQueue(movesToDo, currentBoard, visitedStates, queueAdded, "U");
                    currentBoard.slideDown();
                }
            }
            if (lastMove != 'U') {
                if (currentBoard.slideDown()) {
                    queueAdded = addToQueue(movesToDo, currentBoard, visitedStates, queueAdded, "D");
                    currentBoard.slideDown();
                }
            }
            if (numOfIterations < 2) {
                System.out.println("[" + movesToDo + "\b]");
            }
            if (visitedStates.size() > 100_000){
                System.out.println("There is no solution");
                solved = false;
                break;
            } else if (!movesToDo.isEmpty()) {
                currentBoard = movesToDo.deleteMin();
                queueRemoved++;
            } else {
                System.out.println("Unable to determine if there is a solution");
                solved = false;
                break;
            }
            numOfIterations++;
        }
        if (solved) {
            showHowToSolve(theBoard, currentBoard);
            System.out.println();
            System.out.println("Moves Required: " + currentBoard.getSteps() + "(" + currentBoard.getNumSteps() + ")");
            System.out.println("Queue added=" + queueAdded + " Removed=" + queueRemoved);
        }
        System.out.println();
    }

    private void showHowToSolve(Board originalBoard, Board solvedBoard) {
        System.out.println("Solution");
        System.out.println(originalBoard);
        Board board = new Board(originalBoard.getId());
        String steps = solvedBoard.getSteps();
        for (int i = 0; i < steps.length(); i++) {
            char step = steps.charAt(i);
            System.out.println(step + "==>");
            if (step == 'R') {
                board.slideRight();
                System.out.println(board);
            } else if (step == 'L') {
                board.slideLeft();
                System.out.println(board);
            } else if (step == 'U') {
                board.slideUp();
                System.out.println(board);
            } else if (step == 'D') {
                board.slideDown();
                System.out.println(board);
            }
        }
    }

    private int addToQueue(AVLTree<Board> movesToDo, Board currentBoard, List<String> visitedStates, int queueAdded, String letter) {
        currentBoard.setSteps(letter);
        visitedStates.add(currentBoard.getId());
        movesToDo.insert(currentBoard);
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
