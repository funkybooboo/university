package hashtables;

public class DoubleHashTable<E> extends HashTable<E> {
    @Override
    protected int findPos(E x) {
        numFinds++;
        int currentPos = myhash(x);
        int step = getStep(currentPos);
        while (array[currentPos] != null && !array[currentPos].element.equals(x)) {
            currentPos = (currentPos + step) % array.length;
            step = getStep(currentPos);
            if (currentPos >= array.length) {
                currentPos -= array.length+1;
            }
            numOfProbesOnFinds++;
        }
        return currentPos;
    }

    private int getStep(int currentPos) {
        return 1 + (currentPos % (array.length - 2));
    }
}
