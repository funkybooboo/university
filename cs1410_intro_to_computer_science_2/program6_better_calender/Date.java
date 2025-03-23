public abstract class Date {

    protected int year;
    protected int month;
    protected int day;

    //The following 'get' accessor methods. These aren't used by the sample driver code,
    //but would normally be part of a class like this. These accessor methods are used by the unit tests.
    //These methods return values about the calendar date the object represents,
    // which might be today or it might be something else...whatever the current state of the date object is.
    public String getMonthName(){
        return getMonthName(this.month);
    }

    public int getMonth(){
        return this.month;
    }

    public int getYear(){
        return this.year;
    }

    public int getDayOfMonth(){
        return this.day;
    }

    public abstract boolean isLeapYear(int year);

    public boolean isLeapYear(){
        //A method that returns true/false if the calendar date is part of a leap year
        return isLeapYear(this.year);
    }

    public String printShortDate(){
        //A method that prints the calendar date (without a carriage return) in "mm/dd/yyyy" format
        return this.month + "/" + this.day + "/" + this.year;
    }


    public String printLongDate(){
        //A method that prints the calendar date (without a carriage return) in "Month-name dd, yyyy" format
        return getMonthName() + " " + this.day + ", " + this.year;
    }

    public void addDays(int days){
        //A method that adds a specified number of days to the calendar date
        if(this.month + days > getNumberOfDaysInMonth(this.year, this.month)){
            this.day = days;
        }else {
            this.day++;
        }
    }

    public void subtractDays(int days){
        //A method that subtracts a specified number of days to the calendar date
        if(this.month - days < getNumberOfDaysInMonth(this.year, this.month)){
            this.day = days;
        }else {
            this.day--;
        }
    }

    private int getNumberOfDaysInMonth(int year, int month){
        //A method that returns the number of days in a month (for a year)
        switch (month){
            case 1:
            case 3:
            case 5:
            case 7:
            case 8:
            case 10:
            case 12:
                return 31;
            case 4:
            case 6:
            case 9:
            case 11:
                return 30;
            case 2:
                if (isLeapYear(year)){
                    return 29;
                }else {
                    return 28;
                }
        }
        return 0;
    }

    private int getNumberOfDaysInYear(int year, int month){
        //A method that returns the number of days in a year
        int totalDays = 0;
        for (int countYear = 1800; countYear < year; countYear++){
            if (isLeapYear(year)){
                totalDays += 366;
            }else {
                totalDays += 365;
            }
        }
        for (int countMonth = 1; countMonth< month; countMonth++){
            totalDays += getNumberOfDaysInMonth(year, countMonth);
        }

        return totalDays;
    }

    private String getMonthName(int month){
        //A method that returns the name of a month; first letter capitalized, remainder lowercase
        return switch (month) {
            case 1 -> "January";
            case 2 -> "February";
            case 3 -> "March";
            case 4 -> "April";
            case 5 -> "May";
            case 6 -> "June";
            case 7 -> "July";
            case 8 -> "August";
            case 9 -> "September";
            case 10 -> "October";
            case 11 -> "November";
            case 12 -> "December";
            default -> "error";
        };
    }
}
