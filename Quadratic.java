import java.util.Scanner;
import java.lang.Math;

public class Quadratic {
    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        System.out.println("Welcome to the Quadraticinater");
        System.out.print("Enter a b c: ");
        double a = input.nextDouble();
        double b = input.nextDouble();
        double c = input.nextDouble();

        double discriminant = b * b - 4 * a * c;
        if(discriminant > 0){
            System.out.println("There are two roots for the quadratic equation with these coefficients.");
            double r1 = (-1 * b + (Math.sqrt(b * b - 4 * a * c))) / 2;
            double r2 = (-1 * b - (Math.sqrt(b * b - 4 * a * c))) / 2;
            System.out.println("r1 = " + r1);
            System.out.println("r2 = " + r2);

        }else if(discriminant == 0){
            System.out.println("There is one root for the quadratic equation with these coefficients.");
            double r1 = (-1 * b + (Math.sqrt(b * b - 4 * a * c))) / 2;
            System.out.println("r1 = " + r1);

        }else{
            System.out.println("There are no roots for the quadratic equation with these coefficients.");
        }


    }
}
