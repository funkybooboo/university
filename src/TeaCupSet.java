import java.util.HashMap;
import java.util.Map;

public class TeaCupSet {
    private final Map<Integer, Integer> prices;
    private int[][] priceMatrix;
    private final int largestSetSize;
    private final int numCupsInSet;
    private int largestNumberSize;

    public TeaCupSet(int choice) {
        prices = new HashMap<>();
        if (choice == 1) {
            setOneSetPrices();
        } else {
            setTwoSetPrices();
        }
        largestNumberSize = 2;
        largestSetSize = 12;
        numCupsInSet = 24;
        buildPriceMatrix();
    }

    private void setOneSetPrices() {
        // number of cups -> price for one set
        prices.put(1,1);
        prices.put(2,3);
        prices.put(3,5);
        prices.put(4,9);
        prices.put(5,10);
        prices.put(6,15);
        prices.put(7,17);
        prices.put(8,18);
        prices.put(9,19);
        prices.put(10,22);
        prices.put(11,25);
        prices.put(12,27);
    }

    private void setTwoSetPrices() {
        // number of cups -> price for one set
        prices.put(1,2);
        prices.put(2,5);
        prices.put(3,8);
        prices.put(4,9);
        prices.put(5,10);
        prices.put(6,15);
        prices.put(7,19);
        prices.put(8,23);
        prices.put(9,24);
        prices.put(10,29);
        prices.put(11,30);
        prices.put(12,32);
    }

    public void printPriceMatrix() {
        System.out.println("Top: Number of teacups in the set");
        System.out.println("Left: Largest size of subset to consider");
        int spacing = largestNumberSize+1;
        System.out.printf("%-"+spacing+"d", 0);
        System.out.print("  ");
        for (int i = 1; i <= numCupsInSet; i++) {
            System.out.printf("%-"+spacing+"d", i);
        }
        System.out.println("\n");
        for (int i = 0; i < largestSetSize; i++) {
            System.out.printf("%-"+spacing+"d", i+1);
            System.out.print("  ");
            for (int j = 0; j < numCupsInSet; j++) {
                System.out.printf("%-"+spacing+"d", priceMatrix[i][j]);
            }
            System.out.println();
        }
    }

    private void buildPriceMatrix() {
        priceMatrix = new int[largestSetSize][numCupsInSet];
        for (int i = 0; i < largestSetSize; i++) { priceMatrix[i][0] = prices.get(1); }
        for (int j = 0; j < numCupsInSet; j++) { priceMatrix[0][j] = (j+1)*prices.get(1); }
        for (int i = 1; i < largestSetSize; i++) {
            for (int j = 1; j < numCupsInSet; j++) {
                setPrice(i, j);
                checkForLargestNumberSize(i, j);
            }
        }
    }
    private void setPrice(int i, int j) {
        int largestSetSize = i+1;
        int numCupsInSet = j+1;
        int price1 = priceMatrix[i-1][j];
        int price2 = 0;
        if (largestSetSize <= numCupsInSet) {
            while(numCupsInSet > 0) {
                if (numCupsInSet - largestSetSize > 0) {
                    price2 += prices.get(largestSetSize);
                    numCupsInSet -= largestSetSize;
                } else if (numCupsInSet - largestSetSize < 0) {
                    price2 += priceMatrix[i][numCupsInSet-1];
                    numCupsInSet = 0;
                } else {
                    price2 += prices.get(largestSetSize);
                    numCupsInSet = 0;
                }
            }
        }
        priceMatrix[i][j] = Math.max(price1, price2);
    }

    private void checkForLargestNumberSize(int i, int j) {
        int price;
        price = priceMatrix[i][j];
        if (String.valueOf(price).length() > largestNumberSize) {
            largestNumberSize = String.valueOf(price).length();
        }
    }

    public int getBestPrice(int size) {
        if (size < 1 || size > numCupsInSet) {
            throw new IllegalArgumentException("Invalid num of cups");
        }
        if (size >= 12) {
            return getBestPrice(12, size);
        }
        return getBestPrice(size, size);
    }
    private int getBestPrice(int largestSetSize, int numCupsInSet) {
        return priceMatrix[largestSetSize-1][numCupsInSet-1];
    }

    public String getCupSizes(int size) {
        if (size < 1 || size > numCupsInSet) {
            throw new IllegalArgumentException("Invalid num of cups");
        }
        if (size == 1) {
            return 1+" ";
        }
        if (size > largestSetSize) {
            return getCupSizes(largestSetSize, size);
        }
        return getCupSizes(size, size);
    }
    private String getCupSizes(int largestSetSize, int numCupsInSet) {
        StringBuilder cups = new StringBuilder();
        int thisPrice = priceMatrix[largestSetSize-1][numCupsInSet-1];
        int prevPrice = priceMatrix[largestSetSize-2][numCupsInSet-1];
        while (thisPrice == prevPrice) {
            largestSetSize = largestSetSize - 1;
            thisPrice = priceMatrix[largestSetSize-1][numCupsInSet-1];
            prevPrice = priceMatrix[largestSetSize-2][numCupsInSet-1];
        }
        int totalPrice = thisPrice;
        int setPrice;
        while (totalPrice > 0 && numCupsInSet > 0 && largestSetSize > 0) {
            setPrice = prices.get(largestSetSize);
            if (totalPrice >= setPrice && numCupsInSet >= largestSetSize) {
                totalPrice -= setPrice;
                cups.append(largestSetSize).append(" ");
                numCupsInSet -= largestSetSize;
            } else {
                largestSetSize -= 1;
            }
        }
        return cups.toString().trim();
    }

    private int getSetPrice(String set) {
        int total = 0;
        String[] parts = set.split(" ");
        for (String part : parts) {
            total += prices.get(Integer.parseInt(part));
        }
        return total;
    }

    public static void printChoices(int numCups) {
        System.out.println(numCups);
        printChoices(numCups, "", numCups-1);
    }
    private static void printChoices(int numCups, String soFar, int cupSize) {
        if (cupSize <= 0 || numCups < 0) {
            return;
        }
        if (numCups == 0) {
            System.out.println(soFar);
            return;
        }
        printChoices(numCups-cupSize, soFar + cupSize + " ", cupSize);
        printChoices(numCups, soFar, cupSize-1);
    }

}
