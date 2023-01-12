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
        System.out.println("Working on solving board");
        Queue<State> movesToDo = new Queue<>();
        State currentState = new State(theBoard.getId(), "");
        Board currentBoard = new Board(theBoard.getId());
        Set<String> visitedStates = new HashSet<>();
        int queueAdded = 0;
        int queueRemoved = 0;
        boolean solved = true;

        while (!currentBoard.isSolved(currentState.getId())) {

            if (currentBoard.slideRight() && !visitedStates.contains(currentBoard.getId())) {
                queueAdded = addToQueue(movesToDo, currentState, currentBoard, visitedStates, queueAdded, "R");
                currentBoard.slideLeft();
            }
            if (currentBoard.slideLeft() && !visitedStates.contains(currentBoard.getId())) {
                queueAdded = addToQueue(movesToDo, currentState, currentBoard, visitedStates, queueAdded, "L");
                currentBoard.slideRight();
            }
            if (currentBoard.slideUp() && !visitedStates.contains(currentBoard.getId())) {
                queueAdded = addToQueue(movesToDo, currentState, currentBoard, visitedStates, queueAdded, "U");
                currentBoard.slideDown();
            }
            if (currentBoard.slideDown() && !visitedStates.contains(currentBoard.getId())) {
                queueAdded = addToQueue(movesToDo, currentState, currentBoard, visitedStates, queueAdded, "D");
                currentBoard.slideUp();
            }
            if (movesToDo.getSIZE() > 0) {
                currentState = movesToDo.removeFront();
                currentBoard = new Board(currentState.getId());
                queueRemoved++;
            } else {
                System.out.println("There is no solution");
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

    private int addToQueue(Queue<State> movesToDo, State currentState, Board currentBoard, Set<String> visitedStates, int queueAdded, String letter) {
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
