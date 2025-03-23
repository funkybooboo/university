package hashtables;

public class QuadraticHashTable<E> extends HashTable<E> {
    @Override
    protected int findPos(E x) {
        numFinds++;
        int offset = 1;
        int currentPos = myhash(x);
        while (array[currentPos] != null && !array[currentPos].element.equals(x)) {
            currentPos += offset;  // Compute ith probe
            offset += 2;
            if (currentPos >= array.length) {
                currentPos -= array.length+1;
            }
            numOfProbesOnFinds++;
        }
        return currentPos;
    }
}
