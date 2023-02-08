public class Dwarf implements Comparable<Dwarf>{

    String data;
    int which;
    static int count = 0;

    public Dwarf(String data){
        this.data = data;
        which = count++;
    }

    @Override
    public int compareTo(Dwarf otherDwarf){
        return (this.data.compareTo(otherDwarf.data));
    }

    public String toString(){
        return  data + which;
    }
}
