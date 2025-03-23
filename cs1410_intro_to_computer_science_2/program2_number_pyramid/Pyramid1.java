import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Pyramid1 {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        System.out.print("Enter number of lines: ");
        try {
            String s = scan.next();
            int userNum = Integer.parseInt(s);
            String msg = generatePiramid(userNum);
            System.out.println(msg);
        } catch (Exception e) {
            System.exit(1);
        }
    }

    private static String generatePiramid(int userNum) {
        StringBuilder msg = new StringBuilder();
        for (int i = 1 ; i <= userNum ; i++) {
            List<Integer> numbers = produceLineNumbers(i);
            appendSpaces(msg, userNum - i);
            appendLine(msg, numbers);
        }
        return msg.toString();
    }

    private static void appendSpaces(StringBuilder msg, int count) {
        msg.append(" ".repeat(count*2));
    }

    private static void appendLine(StringBuilder msg, List<Integer> numbers) {
        msg.append(" ");
        for (Integer number : numbers) {
            msg.append(number);
            msg.append(" ");
        }
        msg.append("\n");
    }

    private static List<Integer> produceLineNumbers(int line) {
        List<Integer> numbers = new ArrayList<>();
        for (int i = line ; i >= 1 ; i--) {
            numbers.add(i);
        }
        for (int i = 2 ; i <= line ; i++) {
            numbers.add(i);
        }
        return numbers;
    }
}
