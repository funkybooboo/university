import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

public class TestSched {


    public static void readTasks(String filename,
                          ArrayList<Task> taskX, ArrayList<Task> taskY,
                                 ArrayList<Task> taskZ) {
        // Create lists where base type is different

    }

    public static void main(String args[]) {
        Scheduler s = new Scheduler();
        String [] files = {"tasksetA.txt","tasksetB.txt","tasksetC.txt","tasksetD.txt","tasksetE.txt" };
        for (String f : files) {
            ArrayList<Task> versionX = new ArrayList();    // elements are TaskX
            ArrayList<Task> versionY = new ArrayList();    // elements are TaskY
            ArrayList<Task> versionZ = new ArrayList();    // elements are TaskZ
            readTasks(f, versionX,versionY,versionZ);
            s.makeSchedule("Deadline Priority "+ f, versionX);
            s.makeSchedule("Startime Priority " + f, versionY);
            s.makeSchedule("Wild and Crazy priority " + f, versionZ);
       }

    }
}
