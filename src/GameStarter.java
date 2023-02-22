import java.io.File;
import java.util.Scanner;

public class GameStarter {
    private String fileName;
    private final HashTable<WordInfo> H;

    public GameStarter() {
        H = new HashTable<WordInfo>();
    }

    public int computeScore(WordInfo wi) {
        return 0;
    }

    public void playGame(String filename) {
        fileName = filename;
        System.out.println("FILE " + filename);
        try {
            Scanner sc = new Scanner(new File(filename));
            while (sc.hasNext()) {
                String word = sc.next();
                //Compute score for word
            }
        } catch (Exception e) {
            e.printStackTrace();
        }


    }

    public static void main(String[] args) {
        String[] games = {"game0.txt", "game1.txt", "game2.txt", "game3.txt", "game4.txt"};
        for (String filename : games) {
            GameStarter g = new GameStarter();
            g.playGame(filename);
            System.out.println(g);
        }
    }

}
