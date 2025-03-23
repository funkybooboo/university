import java.util.Scanner;

public class ISBN {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        System.out.println("Welcome to the ISBNinater!");
        System.out.print("Enter the first 9 digits of an ISBN: ");
        int userInput = scan.nextInt();
        int d1 = userInput / 100000000;
        int n1 = userInput - (d1 * 100000000);
        int d2 = n1 / 10000000;
        int n2 = n1 - d2 * 10000000;
        int d3 = n2 / 1000000;
        int n3 = n2 - d3 * 1000000;
        int d4 = n3 / 100000;
        int n4 = n3 - d4 * 100000;
        int d5 = n4 / 10000;
        int n5 = n4 - d5 * 10000;
        int d6 = n5 / 1000;
        int n6 = n5 - d6 * 1000;
        int d7 = n6 / 100;
        int n7 = n6 - d7 * 100;
        int d8 = n7 / 10;
        int d9 = n7 - d8 * 10;

        int d10 = (d1 + d2 * 2 + d3 * 3 + d4 * 4 + d5 * 5 + d6 * 6 + d7 * 7 + d8 * 8 + d9 * 9) % 11;

        String strD1 = String.valueOf(d1);
        String strD2 = String.valueOf(d2);
        String strD3 = String.valueOf(d3);
        String strD4 = String.valueOf(d4);
        String strD5 = String.valueOf(d5);
        String strD6 = String.valueOf(d6);
        String strD7 = String.valueOf(d7);
        String strD8 = String.valueOf(d8);
        String strD9 = String.valueOf(d9);
        String strD10 = String.valueOf(d10);

        if (d10 == 10){
            strD10 = "X";
            System.out.println(strD1 + strD2 + strD3 + strD4 + strD5 + strD6 + strD7 + strD8 + strD9 + strD10);
        }
        else {
            System.out.println(strD1 + strD2 + strD3 + strD4 + strD5 + strD6 + strD7 + strD8 + strD9 + strD10);
        }

    }
}
