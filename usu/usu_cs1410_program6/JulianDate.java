//Julian calendar: If the remainder of the year when divided by 4 is 0

public class JulianDate extends Date{

    public JulianDate(int year, int month, int day){
        this.year = year;
        this.month = month;
        this.day = day;

    }

    public JulianDate(){
        double time = System.currentTimeMillis();
        double timeZoneOffSet = java.util.TimeZone.getDefault().getRawOffset();
        double milli = time - timeZoneOffSet;
        double days = milli / 86400000;
        addDays((int) days);

    }

    public boolean isLeapYear(int year){
        //A method that returns true/false if the specified year is a leap year
        return (year % 400 == 0 || (year % 4 == 0 && year % 100 != 0));
    }

}
