# Introduction

This assignment has you writing various scheduling algorithms, giving you greater insight into understanding how they work and the different results they have on processes. In order to help you concentrate on the scheduling algorithms, you are provided with the simulation platform and the setup and demonstration code for each of the scheduling algorithms. Your task (no pun intended) is to write the implementation for each of the selected scheduling algorithms.

# Assignment

Write the implementation for each of the following scheduling algorithms:

- **First Come, First Served (FCFS)**
- **Shortest Job First (SJF)**
- **Shortest Remaining Time First (SRTF)**
- **Priority**
- **Round Robin (RR)**

The simulator platform (detailed in the next section) requires all scheduler classes to extend the `Scheduler` abstract class.

This class has one method that is implemented to track the number of context switches. A context switch occurs when a process is scheduled and when a process is unscheduled (returned to the ready queue). For example, if process P1 is running and a new process, P2, is scheduled, count one context switch to remove P1 and a second context switch to start P2. You do not have to simulate the time it takes to do a context switch; just track how many occur.

# Simulator Platform

Please refer to the following UML diagram to additionally help you understand the application components: **SchedulerUML**. This diagram shows the UML for only the `ScheduleFCFS` class, but it is the same for the other scheduling classes you have to write.

The code to test the different schedulers is provided. It includes several files. **DO NOT modify these files.**

- **Assign5.java**: This file contains code to demonstrate each of the different scheduling algorithms. For each scheduling algorithm, there is a corresponding `demoX` method. These methods create a `Platform` object, with some number of CPUs, along with a queue of processes to schedule. Finally, an instance of the selected scheduling algorithm is created. The scheduling instance is passed to the `Platform` instance and the simulation is performed. During the simulation, the scheduling code reports back as scheduling events occur using the `Logger` interface. Refer to the sample output for the events to report.

- **Logger.java**: This declares the `Logger` interface. This interface contains a single method used to report events that occur during scheduling. The `Platform` implements the `Logger` interface and is passed to the schedulers through their constructor. When an event should be reported, this interface must be used to report that event, rather than directly using `System.out.print` inside of the scheduler code.

- **Platform.java**: This code contains the core simulation code. It handles notifying the scheduler when a new process has arrived and is available for scheduling. It also loops through each of the CPUs and updates the state of the running process on those CPUs, along with asking the scheduler to update the scheduled process on those CPUs. The platform also tracks how long the simulation has been running and if it is complete. The `Platform` implements the `Logger` interface, used by the schedulers to report scheduling events.

- **Process.java**: This is used to represent a process in the simulation. Each process is initialized with the length of its CPU burst, along with the total execution time and a priority if necessary. The simulation code takes care of updating the state of a process; your scheduling code only needs to use the accessor methods to determine the process state for scheduling decisions.

- **Scheduler.java**: This is an abstract class all schedulers must extend. The details of this class are described in the next section.

    > Note: The second parameter to the `SchedulerRR` class is the time quantum to use.

# Scheduler Abstract Class

The source file `Scheduler.java` contains the `Scheduler` abstract class.

- `int getNumberOfContextSwitches()`: Returns the number of context switches that occur during the simulation. This is invoked by the `Assign5` class.
- `void notifyNewProcess(Process process)`: The simulator platform invokes this whenever a new process arrives and is available to be scheduled.
- `Process update(Process process, int cpu)`: The simulator platform invokes this each time increment. The scheduler should update the scheduling when this method is called.
    - The `process` parameter is the currently running process on that CPU. If no process is running on that CPU, `process` is null.
    - The return value is the process that should be (or continue to be) scheduled.

# Notes

**NO, ZERO, NADA changes to any of the provided code may be made.**

Submit ONLY the following files. Put them in a project folder with the appropriate name, then zip the project folder (not just the individual files).

- `SchedulerFCFS.java`
- `SchedulerSJF.java`
- `SchedulerSRTF.java`
- `SchedulerPriority.java`
- `SchedulerRR.java`

**DON'T SKIP THIS**: For the Priority and Shortest-Remaining-Time-First, you can use a `PriorityQueue` object to make life easy. In fact, you should use it. It will make your output identical to the expected output in cases where the priority/remaining time between two processes is equal. Here is some demo code to help you out. Please read the comments too; they'll provide insight into some methods you may want to use. [ComparatorDemo.zip](#)

  **Download ComparatorDemo.zip**

If your program runs properly with the unmodified starter code, then it will run fine with our test bed. We will drop your files into our project and build and run. There is no threading needed, or desired, for this program. It is a simulation of scheduling, not trying to do anything with threading.

# Example Output

Running Test Jar: `java -jar Assn5Test.jar processes.txt`

The jar file (it's been obfuscated, so don't waste your time) will run the program for you. You can feed in the input file and modify it (keeping the same format) to generate other examples of output to compare your program to. If you do not put in a file name, then it just uses the hardcoded input from the demo code, which should produce the following output:

```
---------------------------------------------------------
Starting First Come, First Served CPU scheduling simulation
Time   0 : CPU 0 > Scheduled P1
Time  24 : CPU 0 > Process P1 burst complete
Time  24 : CPU 0 > Scheduled P2
Time  27 : CPU 0 > Process P2 burst complete
Time  27 : CPU 0 > Scheduled P3
Time  30 : CPU 0 > Process P3 burst complete
Time  30 : CPU 0 > Scheduled P1
Time  54 : CPU 0 > Process P1 burst complete
Time  54 : CPU 0 > Process P1 execution complete
Time  54 : CPU 0 > Scheduled P2
Time  57 : CPU 0 > Process P2 burst complete
Time  57 : CPU 0 > Process P2 execution complete
Time  57 : CPU 0 > Scheduled P3
Time  60 : CPU 0 > Process P3 burst complete
Time  60 : CPU 0 > Process P3 execution complete
Number of context switches: 12
FCFS CPU scheduling simulation complete

---------------------------------------------------------
Starting Shortest Job First CPU scheduling simulation
Time   0 : CPU 0 > Scheduled P4
Time   3 : CPU 0 > Process P4 burst complete
Time   3 : CPU 0 > Process P4 execution complete
Time   3 : CPU 0 > Scheduled P1
Time   9 : CPU 0 > Process P1 burst complete
Time   9 : CPU 0 > Process P1 execution complete
Time   9 : CPU 0 > Scheduled P3
Time  16 : CPU 0 > Process P3 burst complete
Time  16 : CPU 0 > Process P3 execution complete
Time  16 : CPU 0 > Scheduled P2
Time  24 : CPU 0 > Process P2 burst complete
Time  24 : CPU 0 > Process P2 execution complete
Number of context switches: 8
SJF CPU scheduling simulation complete

---------------------------------------------------------
Starting Shortest Remaining Time First CPU scheduling simulation
Time   0 : CPU 0 > Scheduled P1
Time   1 : CPU 0 > Preemptively removed: P1
Time   1 : CPU 0 > Scheduled P2
Time   5 : CPU 0 > Process P2 burst complete
Time   5 : CPU 0 > Process P2 execution complete
Time   5 : CPU 0 > Scheduled P4
Time  10 : CPU 0 > Process P4 burst complete
Time  10 : CPU 0 > Process P4 execution complete
Time  10 : CPU 0 > Scheduled P1
Time  17 : CPU 0 > Process P1 burst complete
Time  17 : CPU 0 > Process P1 execution complete
Time  17 : CPU 0 > Scheduled P3
Time  26 : CPU 0 > Process P3 burst complete
Time  26 : CPU 0 > Process P3 execution complete
Number of context switches: 10
SRTF CPU scheduling simulation complete

---------------------------------------------------------
Starting Priority CPU scheduling simulation
Time   0 : CPU 0 > Scheduled P2
Time   1 : CPU 0 > Process P2 burst complete
Time   1 : CPU 0 > Process P2 execution complete
Time   1 : CPU 0 > Scheduled P5
Time   6 : CPU 0 > Process P5 burst complete
Time   6 : CPU 0 > Process P5 execution complete
Time   6 : CPU 0 > Scheduled P1
Time  16 : CPU 0 > Process P1 burst complete
Time  16 : CPU 0 > Process P1 execution complete
Time  16 : CPU 0 > Scheduled P3
Time  18 : CPU 0 > Process P3 burst complete
Time  18 : CPU 0 > Process P3 execution complete
Time  18 : CPU 0 > Scheduled P4
Time  19 : CPU 0 > Process P4 burst complete
Time  19 : CPU 0 > Process P4 execution complete
Number of context switches: 10
Priority CPU scheduling simulation complete

---------------------------------------------------------
Starting Round Robin CPU scheduling simulation
Time   0 : CPU 0 > Scheduled P1
Time   4 : CPU 0 > Time quantum complete for process P1
Time   4 : CPU 0 > Scheduled P2
Time   7 : CPU 0 > Process P2 burst complete
Time   7 : CPU 0 > Process P2 execution complete
Time   7 : CPU 0 > Scheduled P3
Time  10 : CPU 0 > Process P3 burst complete
Time  10 : CPU 0 > Process P3 execution complete
Time  10 : CPU 0 > Scheduled P1
Time  14 : CPU 0 > Time quantum complete for process P1
Time  14 : CPU 0 > Scheduled P1
Time  18 : CPU 0 > Time quantum complete for process P1
Time  18 : CPU 0 > Scheduled P1
Time  22 : CPU 0 > Time quantum complete for process P1
Time  22 : CPU 0 > Scheduled P1
Time  26 : CPU 0 > Time quantum complete for process P1
Time  26 : CPU 0 > Scheduled P1
Time  30 : CPU 0 > Process P1 burst complete
Time  30 : CPU 0 > Process P1 execution complete
Number of context switches: 16
RR CPU scheduling simulation complete
```
