import java.util.Objects;

public class WordInfo {
    private final String word;
    private final int size;
    private final int sumOfLetterValues;
    private final int lengthValue;
    private int wordFrequency;

    public WordInfo(String word) {
        this.word = word;
        this.size = word.length();
        sumOfLetterValues = computeSumOfletterValues();
        lengthValue = computeLengthValue();
        wordFrequency = 0;
    }

    public int getBonus() {
        if (wordFrequency == 0) {
            return 5;
        }else if (wordFrequency >= 1 && wordFrequency <= 5){
            return 4;
        } else if (wordFrequency >= 6 && wordFrequency <= 10){
            return 3;
        } else if (wordFrequency >= 11 && wordFrequency <= 15){
            return 2;
        } else {
            return 1;
        }
    }

    public void incrementWordFrequency() {
        wordFrequency++;
    }

    public int getWordFrequency() {
        return wordFrequency;
    }

    private int computeLengthValue() {
        int lengthValue;
        if (size <= 2) {
            lengthValue = 0;
        } else if (size >= 8) {
            lengthValue = 6;
        } else {
            lengthValue = size - 2;
        }
        return lengthValue;
    }

    private int computeSumOfletterValues() {
        int sum = 0;
        String tempWord = word.toLowerCase();
        for (int i = 0; i < tempWord.length(); i++) {
            char letter = tempWord.charAt(i);
            switch (letter) {
                case 'a' -> sum += 1;
                case 'b' -> sum += 3;
                case 'c' -> sum += 3;
                case 'd' -> sum += 2;
                case 'e' -> sum += 1;
                case 'f' -> sum += 4;
                case 'g' -> sum += 2;
                case 'h' -> sum += 4;
                case 'i' -> sum += 1;
                case 'j' -> sum += 8;
                case 'k' -> sum += 5;
                case 'l' -> sum += 1;
                case 'm' -> sum += 3;
                case 'n' -> sum += 1;
                case 'o' -> sum += 1;
                case 'p' -> sum += 3;
                case 'q' -> sum += 10;
                case 'r' -> sum += 1;
                case 's' -> sum += 1;
                case 't' -> sum += 1;
                case 'u' -> sum += 1;
                case 'v' -> sum += 4;
                case 'w' -> sum += 4;
                case 'x' -> sum += 8;
                case 'y' -> sum += 4;
                case 'z' -> sum += 10;
            }
        }
        return sum;
    }

    public int computeScore() {
        return getSumOfLetterValues() * getLengthValue() * getBonus();
    }

    public int getLengthValue() {
        return lengthValue;
    }

    public int getSumOfLetterValues() {
        return sumOfLetterValues;
    }

    public String getWord() {
        return word;
    }

    public int getSize() {
        return size;
    }

    @Override
    public boolean equals(Object word2) {
        if (word2 == this){
            return true;
        }
        if (!(word2 instanceof WordInfo w)) {
            return false;
        }
        return (this.word.compareTo(w.word) == 0);
    }

    @Override
    public int hashCode() {
        return Objects.hash(word);
    }

    @Override
    public String toString() {
        return "WordInfo{" + "word='" + word + '\'' + ", size=" + size + ", sumOfLetterValues=" + sumOfLetterValues + ", lengthValue=" + lengthValue + ", wordFrequency=" + wordFrequency + '}';
    }

    public static void main(String[] args){
        WordInfo a = new WordInfo("dog");
        WordInfo b = new WordInfo("dog");
        WordInfo c = new WordInfo("cat");
        int aHash = a.hashCode();
        int bHash = b.hashCode();
        int cHash = c.hashCode();
        if (a.equals(b)) {
            System.out.println("a = b");
        } else {
            System.out.println("a != b");
        }
        if (aHash == bHash) {
            System.out.println("aHash = bHash");
        } else {
            System.out.println("aHash != bHash");
        }
        if (a.equals(c)) {
            System.out.println("a = c");
        } else {
            System.out.println("a != c");
        }
        if (aHash == cHash) {
            System.out.println("aHash = cHash");
        } else {
            System.out.println("aHash != cHash");
        }
    }

}
