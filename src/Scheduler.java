import task.Task;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.List;
import java.util.ArrayList;
import java.util.Scanner;


public class Scheduler {


    public static void readTasks(String filename, List<Task> task1, List<Task> task2, List<Task> task3) {
        // Create lists where base type is different
        try {
            Scanner scanner = new Scanner(new File(filename));
            while (scanner.hasNext()) {
                String[] line = scanner.nextLine().strip().split(" ");
                
            }
        }
        catch (FileNotFoundException e) {
            System.out.println(e.toString());
        }

    }

    public void makeSchedule(String title, List<Task> version) {
        System.out.println(title);

    }

    public static void main(String args[]) {
        Scheduler s = new Scheduler();
        String [] files = {"data/tasksetA.txt", "data/tasksetB.txt", "data/tasksetC.txt", "data/tasksetD.txt", "data/tasksetE.txt"};
        for (String file : files) {
            List<Task> version1 = new ArrayList<>();    // elements are src.TaskX
            List<Task> version2 = new ArrayList<>();    // elements are TaskY
            List<Task> version3 = new ArrayList<>();    // elements are TaskZ
            readTasks(file, version1,version2,version3);
            s.makeSchedule("Deadline Priority "+ file, version1);
            s.makeSchedule("Startime Priority " + file, version2);
            s.makeSchedule("Wild and Crazy priority " + file, version3);
       }

    }
}
