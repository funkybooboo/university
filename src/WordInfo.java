import java.util.Objects;

public class WordInfo {
    private final String word;
    private final int size;

    public WordInfo(String word) {
        this.word = word;
        this.size = word.length();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        WordInfo wordInfo = (WordInfo) o;
        return size == wordInfo.size && Objects.equals(word, wordInfo.word);
    }

    @Override
    public int hashCode() {
        return Objects.hash(word, size);
    }
}
