import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Pyramid2 {
    public static void main(String[] args) {
        int userNum;
        if(args.length == 0){
            Scanner scan = new Scanner(System.in);
            System.out.print("Enter number of lines: ");
            userNum = scan.nextInt();
        }
        else{
            userNum = Integer.parseInt(args[0]);
        }
        try {
            String msg = generatePiramid(userNum);
            System.out.println(msg);
        } catch (Exception e) {
            System.exit(1);
        }
    }

    private static String generatePiramid(int userNum) {
        StringBuilder msg = new StringBuilder();
        List<List<Integer>> allRows = new ArrayList<>();
        int maxLen = 0;
        List<Integer> middleNums = getMidNums(userNum);
        for (Integer i : middleNums) {
            List<Integer> numbers = produceLineNumbers(i);
            allRows.add(numbers);
            int maxLen2 = getLenOfLarNum(numbers);
            if (maxLen2 > maxLen){
                maxLen = maxLen2;
            }
        }
        int r = 1;
        for (List<Integer> row : allRows) {
            appendLine(msg, row, maxLen, userNum - r);
            r++;
        }
        return msg.toString();
    }

    private static void appendSpaces(StringBuilder msg, int count) {
        msg.append(" ".repeat(count));
    }

    private static void appendLine(StringBuilder msg, List<Integer> numbers, int maxLen, int r) {
        appendSpaces(msg, r * (maxLen + 1));
        for (Integer number : numbers) {
            int len = String.valueOf(number).length();
            appendSpaces(msg, maxLen - len + 1);
            msg.append(number);
        }
        msg.append("\n");
    }

    private static List<Integer> produceLineNumbers(int middleNum) {
        List<Integer> numbers = new ArrayList<>();
        for (int i = 1; i < middleNum; i*=2) {
            numbers.add(i);
        }
        numbers.add(middleNum);
        for (int j = middleNum / 2; j >= 1; j/=2) {
            numbers.add(j);
        }
        return numbers;
    }

    private static List<Integer> getMidNums(int line) {
        List<Integer> middleNums = new ArrayList<>();
        int x = 1;
        for (int i = 1; i <= line; i++){
            middleNums.add(x);
            x *= 2;
        }
        return middleNums;
    }

    private static int getLenOfLarNum(List<Integer> list){
        int max = 0;
        for (Integer i : list) {
            if (String.valueOf(i).length() > max){
                max = String.valueOf(i).length();
            }
        }
        return max;
    }
}
