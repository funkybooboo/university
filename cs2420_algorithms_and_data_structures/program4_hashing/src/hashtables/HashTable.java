package hashtables;

import java.util.Arrays;

public abstract class HashTable<E> {
    protected static class HashEntry<E> {
        public E element;
        public boolean isActive;

        public HashEntry(E e) {
            this(e, true);
        }

        public HashEntry(E e, boolean i) {
            element = e;
            isActive = i;
        }
    }

    protected static final int DEFAULT_TABLE_SIZE = 101;
    protected HashEntry<E>[] array;
    protected int occupiedCt;
    protected int currentActiveEntries;
    protected int numFinds = 0;
    protected int numOfProbesOnFinds = 0;
    protected int numItemsInTable = 0;

    public HashTable() {
        this(DEFAULT_TABLE_SIZE);
    }

    public HashTable(int size) {
        allocateArray(size);
        makeEmpty();
    }

    abstract int findPos(E x);

    public void printStats() {
        System.out.println("HashTable Statistics");
        System.out.println("\tNumber of finds: "+this.getNumFinds());
        System.out.println("\tNumber of probes: "+this.getNumOfProbesOnFinds());
        System.out.println("\tNumber of items stored: "+this.getNumItemsInTable());
        System.out.println("\tPhysical length: "+this.getPhysicalLength());
        this.printFirst20Items();
        System.out.println("\n------------------------------\n");
    }

    public int getNumItemsInTable() {
        int count = 0;
        for (HashEntry<E> e : array) {
            if (e == null) continue;
            count++;
        }
        return count;
    }

    public void printFirst20Items() {
        System.out.println("\tContests of the first 20 non-null entries");
        int count = 1;
        for (HashEntry<E> e : array) {
            if (e == null) continue;
            if (count > 20) break;
            System.out.println("\t\t"+count + " [ " + e.element.toString() + ", active:" + e.isActive + " ] ");
            count++;
        }
    }

    public int getNumFinds() {
        return numFinds;
    }

    public int getNumOfProbesOnFinds() {
        return numOfProbesOnFinds;
    }

    public int getPhysicalLength() {
        return array.length;
    }

    public boolean insert(E x) {
        int currentPos = findPos(x);
        if (isActive(currentPos)) {
            return false;
        }
        array[currentPos] = new HashEntry<>(x, true);
        currentActiveEntries++;
        if (++occupiedCt > array.length / 2) {
            rehash();
        }
        return true;
    }

    public boolean remove(E x) {
        int currentPos = findPos(x);
        if (isActive(currentPos)) {
            array[currentPos].isActive = false;
            currentActiveEntries--;
            return true;
        } else {
            return false;
        }
    }

    public boolean contains(E x) {
        int currentPos = findPos(x);
        return isActive(currentPos);
    }

    public E find(E x) {
        int currentPos = findPos(x);
        if (!isActive(currentPos)) {
            return null;
        } else {
            return array[currentPos].element;
        }
    }

    public String toString(int limit) {
        StringBuilder sb = new StringBuilder();
        int ct = 0;
        for (int i = 0; i < array.length && ct < limit; i++) {
            if (array[i] != null && array[i].isActive) {
                sb.append(i).append(": ").append(array[i].element).append("\n");
                ct++;
            }
        }
        return sb.toString();
    }

    protected void rehash() {
        HashEntry<E>[] oldArray = array;
        allocateArray(2 * oldArray.length);
        occupiedCt = 0;
        currentActiveEntries = 0;
        for (HashEntry<E> entry : oldArray) {
            if (entry != null && entry.isActive) {
                insert(entry.element);
            }
        }
    }

    public int size() {
        return currentActiveEntries;
    }

    public int capacity() {
        return array.length;
    }

    protected boolean isActive(int currentPos) {
        return array[currentPos] != null && array[currentPos].isActive;
    }

    public void makeEmpty() {
        occupiedCt = 0;
        Arrays.fill(array, null);
    }

    protected int myhash(E x) {
        int hashVal = x.hashCode();
        hashVal %= array.length;
        if (hashVal < 0) {
            hashVal += array.length;
        }
        return hashVal;
    }

    protected void allocateArray(int arraySize) {
        array = new HashEntry[nextPrime(arraySize)];
    }

    public static int nextPrime(int startingNumber) {
        if (startingNumber % 2 == 0) {
            startingNumber++;
        }
        while (!isPrime(startingNumber)) {
            startingNumber += 2;
        }
        return startingNumber;
    }

    protected static boolean isPrime(int n) {
        if (n == 2 || n == 3) {
            return true;
        }
        if (n == 1 || n % 2 == 0) {
            return false;
        }
        for (int i = 3; i * i <= n; i += 2) {
            if (n % i == 0) {
                return false;
            }
        }
        return true;
    }

    public void print(String label) {
        System.out.println(label);
        for (int i = 0; i < array.length; i++) {
            HashEntry<E> entry = array[i];
            if (entry == null) {
                System.out.println(i + " [ ]");
            } else {
                System.out.println(i + " [ " + entry.element + " " + entry.isActive + " ]");
            }
        }
    }

    public static void main(String[] args) {
        String[] data = {"queue", "stack", "zion", "consider", "minute", "accord", "committee", "dog", "fish", "queue", "win", "consider", "lose", "queue", "draw", "queue"};
        HashTable<String> hashTable = new QuadraticHashTable<>();
        for (String s : data) {
            hashTable.insert(s);
        }
        hashTable.print("insert data");
        hashTable.remove("zion");
        hashTable.print("remove zion");
    }

}
