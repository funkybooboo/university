// "Every year that is exactly divisible by four is a leap year,
// except for years that are exactly divisible by 100,
// but these centurial years are leap years if they are exactly divisible by 400".

public class GregorianDate extends Date{

    public GregorianDate(int year, int month, int day){
        this.year = year;
        this.month = month;
        this.day = day;

    }

    public GregorianDate(){
        double time = System.currentTimeMillis();
        double timeZoneOffSet = java.util.TimeZone.getDefault().getRawOffset();
        double milli = time - timeZoneOffSet;
        double days = milli / 86400000;
        addDays((int) days);

    }


    public boolean isLeapYear(int year){
        //A method that returns true/false if the specified year is a leap year
        return (year % 400 == 0) || ((year % 100) != 0 && (year % 4 == 0));

    }

}
