public class Assignment4 {

    // I didnt see that you put any instructions on exact data
    // so I just made up numbers but feel free to change values for global veribles

    private static final int ARRAY_SIZE_START = 0;
    private static final int MAX_ARRAY_SIZE = 20000;
    private static final int MAX_VALUE = 1000000;
    private static final int ARRAY_SIZE_INCREMENT = 1;
    private static final int NUMBER_OF_SAMPLES = 5;
    private static final int NUMBER_SEARCHES = 10000;
    private static final int NUMBER_TO_SEARCH = 459831; // TODO - change 459831 to actual value

    public static void main(String[] args) {
        demoLinearSearchUnsorted();
        demoLinearSearchSorted();
        demoBinarySearchSelectionSort();
        demoBinarySearchFastSort();
    }

    public static void demoLinearSearchUnsorted() {
        System.out.println("--- Linear Search Timing (unsorted) ---");
        for (int i = 0 ; i < NUMBER_OF_SAMPLES ; i++) {
            long startTime = System.currentTimeMillis();
            int[] numbers = generateNumbers(MAX_ARRAY_SIZE * (i+1), MAX_VALUE);
            int timesFound = 0;
            for (int j = ARRAY_SIZE_START ; j < NUMBER_SEARCHES ; j = j + ARRAY_SIZE_INCREMENT) {
                boolean found = linearSearch(numbers, NUMBER_TO_SEARCH);
                if (found) {
                    timesFound++;
                }
            }
            long searchTime = System.currentTimeMillis() - startTime;
            System.out.println("Number of items       : " + numbers.length);
            System.out.println("Times value was found : " + timesFound);
            System.out.println("Total search time     : " + searchTime + " ms");
            System.out.println();
        }
    }

    public static void demoLinearSearchSorted() {
        System.out.println("--- Linear Search Timing (Selection Sort) ---");
        for (int i = 0 ; i < NUMBER_OF_SAMPLES ; i++) {
            long startTime = System.currentTimeMillis();
            int[] numbers = generateNumbers(MAX_ARRAY_SIZE * (i+1), MAX_VALUE);
            selectionSort(numbers);
            int timesFound = 0;
            for (int j = ARRAY_SIZE_START ; j < NUMBER_SEARCHES ; j = j + ARRAY_SIZE_INCREMENT) {
                boolean found = linearSearch(numbers, NUMBER_TO_SEARCH);
                if (found) {
                    timesFound++;
                }
            }
            long searchTime = System.currentTimeMillis() - startTime;
            System.out.println("Number of items       : " + numbers.length);
            System.out.println("Times value was found : " + timesFound);
            System.out.println("Total search time     : " + searchTime + " ms");
            System.out.println();
        }
    }

    public static void demoBinarySearchSelectionSort() {
        System.out.println("--- Binary Search Timing (Selection Sort) ---");
        for (int i = 0 ; i < NUMBER_OF_SAMPLES ; i++) {
            long startTime = System.currentTimeMillis();
            int[] numbers = generateNumbers(MAX_ARRAY_SIZE * (i+1), MAX_VALUE);
            selectionSort(numbers);
            int timesFound = 0;
            for (int j = ARRAY_SIZE_START ; j < NUMBER_SEARCHES ; j = j + ARRAY_SIZE_INCREMENT) {
                boolean found = binarySearch(numbers, NUMBER_TO_SEARCH);
                if (found) {
                    timesFound++;
                }
            }
            long searchTime = System.currentTimeMillis() - startTime;
            System.out.println("Number of items       : " + numbers.length);
            System.out.println("Times value was found : " + timesFound);
            System.out.println("Total search time     : " + searchTime + " ms");
            System.out.println();
        }
    }

    public static void demoBinarySearchFastSort() {
        System.out.println("--- Binary Search Timing (Arrays.sort) ---");
        for (int i = 0 ; i < NUMBER_OF_SAMPLES ; i++) {
            long startTime = System.currentTimeMillis();
            int[] numbers = generateNumbers(MAX_ARRAY_SIZE * (i+1), MAX_VALUE);
            fastSort(numbers, ARRAY_SIZE_START, MAX_VALUE);
            int timesFound = 0;
            for (int j = ARRAY_SIZE_START ; j < NUMBER_SEARCHES ; j = j + ARRAY_SIZE_INCREMENT) {
                boolean found = linearSearch(numbers, NUMBER_TO_SEARCH);
                if (found) {
                    timesFound++;
                }
            }
            long searchTime = System.currentTimeMillis() - startTime;
            System.out.println("Number of items       : " + numbers.length);
            System.out.println("Times value was found : " + timesFound);
            System.out.println("Total search time     : " + searchTime + " ms");
            System.out.println();
        }
    }

    public static int[] generateNumbers(int howMany, int maxValue) {
        int[] a = new int[howMany];
        for (int i = 0 ; i < howMany ; i++) {
            a[i] = (int)(Math.random() * (double)maxValue);
        }
        return a;
    }

    public static boolean linearSearch(int[] data, int search) {
        for (int datum : data) {
            if (datum == search) {
                return true;
            }
        }
        return false;
    }

    public static boolean linearSearchSorted(int[] data, int search) {
        for (int datum : data) {
            if (datum == search) {
                return true;
            }
        }
        return false;
    }

    public static boolean binarySearch(int[] data, int search) {
        int first = 0;
        int last = data.length-1;
        int mid = (first + last)/2;
        while( first <= last ){
            if ( data[mid] < search ){
                first = mid + 1;
            }else if ( data[mid] == search ){
                return true;
            }else{
                last = mid - 1;
            }
            mid = (first + last)/2;
        }
        return false;
    }

    public static void selectionSort(int[] data) {
        for(int i=0;i<data.length; i++) {
            int minIndex = i;
            for(int j=i+1;j<data.length; j++) {
                if(data[j]<data[minIndex]) {
                    minIndex = j;
                }
            }
            int temp = data[i];
            data[i] = data[minIndex];
            data[minIndex] = temp;
        }
    }

    public static void fastSort(int[] data, int low, int high) {
        if(low >= high) return;
        int pivotPosition = partition(data, low, high);
        fastSort(data,low, pivotPosition-1);
        fastSort(data, pivotPosition+1, high);
    }

    public static int partition(int[] arr, int low, int high) {
        int pivot = arr[high];
        int left = low, right = high-1;
        while(left < right) {
            while(arr[left]<pivot) {
                left++;
            }
            while(arr[right]>pivot) {
                right--;
            }
            if(left >= right) {
                break;
            }
            int temp = arr[left];
            arr[left] = arr[right];
            arr[right] = temp;
        }
        int temp = arr[left];
        arr[left] = arr[high];
        arr[high] = temp;
        return left;
    }

}
