import hashtables.DoubleHashTable;
import hashtables.HashTable;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class GameStarter {
    private final HashTable<WordInfo> H;
    private final HashTable<String> DICTIONARY;

    public GameStarter() {
        H = new DoubleHashTable<>();
        DICTIONARY = new DoubleHashTable<>();
        try {
            Scanner scanner = new Scanner(new File("input/dictionary.txt"));
            while (scanner.hasNext()) {
                DICTIONARY.insert(scanner.next());
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public void playGame(String filename) {
        System.out.println("FILE " + filename);
        int gameScore = 0;
        try {
            getValidWords(filename);
            gameScore = getGameScore(filename, gameScore);
        } catch (Exception e) {
            e.printStackTrace();
        }
        printGameResults(gameScore);
    }

    private int getGameScore(String filename, int gameScore) throws FileNotFoundException {
        Scanner scanner = new Scanner(new File(filename));
        while (scanner.hasNext()) {
            String word = scanner.next();
            if (!DICTIONARY.contains(word)) continue;
            WordInfo wordInfo = H.find(new WordInfo(word));
            gameScore += wordInfo.computeScore();
        }
        return gameScore;
    }

    private void getValidWords(String filename) throws FileNotFoundException {
        Scanner scanner = new Scanner(new File(filename));
        while (scanner.hasNext()) {
            String word = scanner.next();
            if (!DICTIONARY.contains(word)) continue;
            WordInfo wordInfo = new WordInfo(word);
            if (!H.insert(wordInfo)) {
                H.find(wordInfo).incrementWordFrequency();
            }
        }
    }

    private void printGameResults(int gameScore) {
        System.out.println("Game score: "+gameScore);
        H.printStats();
    }

    public static void main(String[] args) {
        String[] games = {"input/game0.txt", "input/game1.txt", "input/game2.txt", "input/game3.txt", "input/game4.txt"};
        for (String filename : games) {
            GameStarter g = new GameStarter();
            g.playGame(filename);
        }
    }

}
