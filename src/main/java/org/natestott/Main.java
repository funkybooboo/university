package org.natestott;

import org.natestott.numberComputer.NumberComputer;
import org.natestott.numberComputer.PiComputer;

/**
 * The Main class is the entry point of the application.
 * It initializes the NumberComputer implementation for computing Pi
 * and triggers the computation.
 *
 * @author Nate Stott
 */
public class Main {

    /**
     * The main method is the entry point for the Java application.
     * It creates an instance of the PiComputer, which implements
     * the NumberComputer interface, and calls the method to compute
     * the value of Pi and print it to the console.
     *
     * @param args Command-line arguments passed to the program (not used).
     */
    public static void main(String[] args) {
        NumberComputer piComputer = new PiComputer();
        piComputer.computeNumberAndPrint();
    }
}
