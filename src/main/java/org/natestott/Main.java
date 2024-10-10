package org.natestott;

import org.natestott.numberComputer.NumberComputer;
import org.natestott.numberComputer.PiComputer;

public class Main {
    public static void main(String[] args) {
        NumberComputer piComputer = new PiComputer();
        piComputer.computeNumberAndPrint();
    }
}
