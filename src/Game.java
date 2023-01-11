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
        System.out.println("Working on solving board");
        Queue<State> movesToDo = new Queue<>();
        State currentState = new State(theBoard.getId(), "");
        List<State> visitedState = new ArrayList<>();
        Board currentBoard = new Board(theBoard.getId());
        int queueAdded = 0;
        int queueRemoved = 0;

        while (!currentBoard.isSolved(currentState.getId())) {
            if (currentBoard.slideRight() && !visitedState.contains(currentState)) {
                State moveRightState = new State(currentBoard.getId(), currentState.getSteps() + "R");
                visitedState.add(moveRightState);
                movesToDo.add(moveRightState);
                queueAdded++;
                currentBoard.slideLeft();
            }
            if (currentBoard.slideLeft() && !visitedState.contains(currentState)) {
                State moveLeftState = new State(currentBoard.getId(), currentState.getSteps() + "L");
                visitedState.add(moveLeftState);
                movesToDo.add(moveLeftState);
                queueAdded++;
                currentBoard.slideRight();
            }
            if (currentBoard.slideUp() && !visitedState.contains(currentState)) {
                State moveUpState = new State(currentBoard.getId(), currentState.getSteps() + "U");
                visitedState.add(moveUpState);
                movesToDo.add(moveUpState);
                queueAdded++;
                currentBoard.slideDown();
            }
            if (currentBoard.slideDown() && !visitedState.contains(currentState)) {
                State moveDownState = new State(currentBoard.getId(), currentState.getSteps() + "D");
                visitedState.add(moveDownState);
                movesToDo.add(moveDownState);
                queueAdded++;
                currentBoard.slideUp();
            }
            currentState = movesToDo.removeFront();
            currentBoard = new Board(currentState.getId());
            queueRemoved++;
        }

        System.out.println("Moves Required: " + currentState.getSteps() + "(" + currentState.getSteps().length() + ")");
        System.out.println("Queue added=" + queueAdded + " Removed=" + queueRemoved);
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
