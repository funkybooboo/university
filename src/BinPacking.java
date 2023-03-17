import heap.heap_sort.HeapSort;
import heap.heap_sort.MaxHeapSort;
import heap.heap_sort.MinHeapSort;
import heap.leftist_heap.LeftistHeap;
import heap.leftist_heap.MaxLeftistHeap;
import heap.leftist_heap.MinLeftistHeap;

import java.util.Arrays;
import java.util.Random;

public class BinPacking {

    private static final int BINSIZE = 100;
    private final int[] requests;

    public BinPacking(int size){
        Random rand  = new Random(size); //Seed will cause the same sequence of numbers to be generated each test
        requests = new  int[size];
        for (int i = 0; i < size; i++){
            requests[i] = rand.nextInt(BINSIZE)+1;
        }
        if (size <= 100){
            System.out.println("Size " + size + " " + Arrays.toString(requests));
        }
    }

    // put files into disks as they come on "the line"
    private int scheduleOnLineWorstFit(String title) {
        LeftistHeap<Disk> maxLeftistHeap = new MaxLeftistHeap<>();
        int totalFileSizesProcessed = 0;
        for (int i = 0; i < requests.length; i++) {
            int fileSize = requests[i];
            totalFileSizesProcessed += fileSize;
            if (maxLeftistHeap.isEmpty()) {
                createNewDisk(maxLeftistHeap, i, fileSize);
            } else {
                Disk disk = maxLeftistHeap.delete();
                if (disk.remainingSpace >= fileSize) {
                    disk.add(fileSize);
                    maxLeftistHeap.insert(disk);
                } else {
                    maxLeftistHeap.insert(disk);
                    createNewDisk(maxLeftistHeap, i, fileSize);
                }
            }
        }
        return printReport(maxLeftistHeap, title, totalFileSizesProcessed);
    }

    private int printReport(LeftistHeap<Disk> maxLeftistHeap, String title, int totalFileSizesProcessed) {
        int minNumberOfDisks = totalFileSizesProcessed / BINSIZE;
        int size = requests.length;
        int numDisksUsed = 0;
        System.out.println(title);

        while (!maxLeftistHeap.isEmpty()) {
            Disk disk = maxLeftistHeap.delete();
            if (size <= 100) {
                System.out.println("\t"+disk.toString());
            }
            numDisksUsed++;
        }

        System.out.println("\tNum of files on the line = "+size);
        System.out.println("\tMinimum number of disks required "+minNumberOfDisks);
        System.out.println("\tNum disks used: "+numDisksUsed);
        return numDisksUsed;
    }

    private void createNewDisk(LeftistHeap<Disk> maxLeftistHeap, int i, int fileSize) {
        Disk disk = new Disk(i+1, BINSIZE);
        disk.add(fileSize);
        maxLeftistHeap.insert(disk);
    }

    // take files off "the line" sort them and then put them back onto "the line"
    private int scheduleOffLineWorstFit() {
        HeapSort<Integer> maxHeapSort = new MaxHeapSort<>();
        int size = requests.length;
        Integer[] tempRequests = new Integer[requests.length];
        for (int i = 0; i < size; i++) {
            tempRequests[i] = requests[i];
        }
        maxHeapSort.sort(tempRequests);
        for (int i = 0; i < size; i++) {
            requests[i] = tempRequests[i];
        }
        if (size <= 100){
            System.out.println(Arrays.toString(requests));
        }
        return scheduleOnLineWorstFit("Decreasing Worst Fit Disk Packing");
    }

    public static void main (String[] args){
        int[] fileSizes = {10, 20, 100, 500, 10000, 100000};
        for (int size :fileSizes){
            BinPacking binPacking = new BinPacking(size);
            int scheduleOnLineWorstFitNumberOfDisksUsed = binPacking.scheduleOnLineWorstFit("OnLine worst Fit Disk Packing");
            System.out.println();
            int scheduleOffLineWorstFitNumberOfDisksUsed = binPacking.scheduleOffLineWorstFit();
            int better = scheduleOnLineWorstFitNumberOfDisksUsed-scheduleOffLineWorstFitNumberOfDisksUsed;
            System.out.println();
            System.out.println("When the files are sorted they use "+better+" less disks");
            double percent = ((double) (scheduleOnLineWorstFitNumberOfDisksUsed-scheduleOffLineWorstFitNumberOfDisksUsed) / (double) scheduleOffLineWorstFitNumberOfDisksUsed)*100;
            System.out.printf("That's %.2f", percent);
            System.out.println("% more efficient");

            System.out.println("\n");
            System.out.println("------------------------");
            System.out.println("\n");
        }
    }

}
