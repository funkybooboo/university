import java.util.*;

public class SliderGame {
    Board theBoard;
    String originalBoardID;
    String boardName;

    public SliderGame() {
        theBoard = new Board();
    }

    public SliderGame(Board board) {
        theBoard = board;
    }

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
        Board boardA = aStarSolve(false);
        Board boardB = bruteForceSolve(false);
        if (!boardA.isSolved(boardA.getId()) && !boardB.isSolved(boardB.getId())) {
            System.out.println();
            System.out.println("A* and Brute didn't find a solution");
            System.out.println();
            System.out.println("----------------------------");
        } else if (boardA.isSolved(boardA.getId()) && !boardB.isSolved(boardB.getId())) {
            System.out.println();
            System.out.println("A* and Brute did not have the same solution");
            System.out.println();
            System.out.print("A* ");
            Board boardCopyA = new Board(boardA.getId(), boardA.getSteps());
            showHowToSolve(theBoard, boardCopyA);
            System.out.println("----------------------------");
            System.out.println();
            System.out.println("Brute did not have a solution");
            System.out.println();
            System.out.println("----------------------------");
        } else if (!boardA.isSolved(boardA.getId()) && boardB.isSolved(boardB.getId())) {
            System.out.println();
            System.out.println("A* and Brute did not have the same solution");
            System.out.println();
            System.out.println("A* did not have a solution");
            System.out.println();
            System.out.println("----------------------------");
            System.out.print("Brute ");
            Board boardCopyB = new Board(boardB.getId(), boardB.getSteps());
            showHowToSolve(theBoard, boardCopyB);
            System.out.println("----------------------------");
        } else {
            if (boardA.getSteps().equals(boardB.getSteps())) {
                Board boardCopy = new Board(boardA.getId(), boardA.getSteps());
                showHowToSolve(theBoard, boardCopy);
                System.out.println("----------------------------");
            } else {
                System.out.println("A* and Brute did not have the same solution");
                System.out.println();
                System.out.print("A* ");
                Board boardCopyA = new Board(boardA.getId(), boardA.getSteps());
                showHowToSolve(theBoard, boardCopyA);
                System.out.println("----------------------------");
                System.out.print("Brute ");
                Board boardCopyB = new Board(boardB.getId(), boardB.getSteps());
                showHowToSolve(theBoard, boardCopyB);
                System.out.println("----------------------------");
            }
        }
    }

    /**
     * Create a random board (which is solvable) by jumbling jumnbleCount times.
     * Solve
     * @param label Name of board (for printing)
     * @param jumbleCount number of random moves to make in creating a board
     */
    public void playRandom(String label, int jumbleCount) {
        theBoard.makeBoard(jumbleCount);
        System.out.println(label + "\n" + theBoard);
        playGiven(label, theBoard);
    }

    // PROGRAM 3 SOLVE
    public void aStarSolve() {
        aStarSolve(true);
    }

    private Board aStarSolve(boolean show) {
        System.out.println("A* Solve");
        Store<Board> movesToDo = new AVLTree<>();
        return solve(show, movesToDo);
    }

    // PROGRAM 1 SOLVE
    public void bruteForceSolve() {
        bruteForceSolve(true);
    }

    private Board bruteForceSolve(boolean show) {
        System.out.println("Brute Solve");
        Store<Board> movesToDo = new Queue<>();
        return solve(show, movesToDo);
    }

    private Board solve(boolean show, Store<Board> movesToDo) {
        Board currentBoard = new Board(theBoard.getId(), "");
        List<String> visitedStates = new ArrayList<>();
        int queueAdded = 1;
        int queueRemoved = 0;
        int numLoops = 0;
        visitedStates.add(currentBoard.getId());
        while (!currentBoard.isSolved(currentBoard.getId())) {
            char lastMove = currentBoard.getLast();
            if (lastMove != 'L') {
                if (currentBoard.slideRight()) {
                    Board boardCopy = new Board(currentBoard.getId(), currentBoard.getSteps());
                    queueAdded = addToQueue(movesToDo, boardCopy, visitedStates, queueAdded, "R");
                    currentBoard.slideLeft();
                }
            }
            if (lastMove  != 'R') {
                if (currentBoard.slideLeft()) {
                    Board boardCopy = new Board(currentBoard.getId(), currentBoard.getSteps());
                    queueAdded = addToQueue(movesToDo, boardCopy, visitedStates, queueAdded, "L");
                    currentBoard.slideRight();
                }
            }
            if (lastMove != 'D') {
                if (currentBoard.slideUp()) {
                    Board boardCopy = new Board(currentBoard.getId(), currentBoard.getSteps());
                    queueAdded = addToQueue(movesToDo, boardCopy, visitedStates, queueAdded, "U");
                    currentBoard.slideDown();
                }
            }
            if (lastMove != 'U') {
                if (currentBoard.slideDown()) {
                    Board boardCopy = new Board(currentBoard.getId(), currentBoard.getSteps());
                    queueAdded = addToQueue(movesToDo, boardCopy, visitedStates, queueAdded, "D");
                    currentBoard.slideUp();
                }
            }
            if (numLoops > 500_000){
                System.out.println("Queue added=" + queueAdded + " Removed=" + queueRemoved);
                return new Board(currentBoard.getId(), currentBoard.getSteps());
            } else if (movesToDo.isNotEmpty()) {
                currentBoard = movesToDo.deleteMin();
                queueRemoved++;
            }
            numLoops++;
        }
        Board boardCopy = new Board(currentBoard.getId(), currentBoard.getSteps());
        if (show) {
            showHowToSolve(theBoard, boardCopy);
            System.out.println();
        }
        System.out.println("Moves Required: " + currentBoard.getSteps() + "(" + currentBoard.getNumSteps() + ")");
        System.out.println("Queue added=" + queueAdded + " Removed=" + queueRemoved);
        System.out.println();
        return boardCopy;
    }

    private int addToQueue(Store<Board> movesToDo, Board currentBoard, List<String> visitedStates, int queueAdded, String letter) {
        currentBoard.setSteps(letter);
        visitedStates.add(currentBoard.getId());
        movesToDo.insert(currentBoard);
        return ++queueAdded;
    }

    private void showHowToSolve(Board originalBoard, Board solvedBoard) {
        System.out.println("Solution");
        System.out.println(originalBoard);
        Board board = new Board(originalBoard.getId(), "");
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
}
