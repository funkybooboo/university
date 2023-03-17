import java.util.Arrays;
import java.util.Random;

public class BinPacking {

    static int BINSIZE = 100;
    int[] requests;

    public BinPacking(int size){
        Random rand  = new Random(size); //Seed will cause the same sequence of numbers to be generated each test
        requests = new  int[size];
        for (int i = 0; i < size; i++){
            requests[i] = rand.nextInt(BINSIZE)+1;
        }
        if (size <= 500){
            System.out.println("Size " + size + " " + Arrays.toString(requests));
        }
    }

    private void scheduleWorstFit() {

    }

    private void scheduleOfflineWorstFit() {

    }

    public static void main (String[] args){
       int[] fileSizes = {10, 20, 100, 500,10000,100000};
        for (int size :fileSizes){
            BinPacking b = new BinPacking(size);
            b.scheduleWorstFit();
            b.scheduleOfflineWorstFit();
        }
    }

}
