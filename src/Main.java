import java.util.Scanner;

public class Main {

    public static void main(String[] args) {

        //String[] games = {"123740658","102453786", "023156478", "413728065", "145236078", "123456870"};
        String[] games = {"102453786", "123740658", "023156478", "413728065", "145236078", "123456870"};
        String[] gameNames = {"Easy Board", "Game1", "Game2", "Game3", "Game4", "Game5 No Solution"};
        Game game = new Game();
        Scanner scanner = new Scanner(System.in);
        Board board;
        String response;
        for (int i = 0; i < games.length; i++) {
            board = new Board(games[i]);
            game.playGiven(gameNames[i], board);
            System.out.println("Click any key to continue\n");
            scanner.nextLine();
        }
        boolean playAgain = true;
        int howMuchJumblingInRandomBoard = 18;
        while (playAgain) {
            game.playRandom("Random Board", howMuchJumblingInRandomBoard);
            System.out.println("Play Again?  Answer Y for yes\n");
            response = scanner.nextLine().toUpperCase();
            playAgain = (!response.equals("")) && (response.charAt(0) == 'Y');
        }

    }
}
