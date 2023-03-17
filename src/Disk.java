public class Disk {

    String contents;
    int remainingSpace;
    int id;

    public Disk(int id, int maxSize){
       contents="";
       remainingSpace = maxSize;
       this.id = id;
    }

    public boolean add(int oneFileSize){
        if (remainingSpace<oneFileSize) return false;
        remainingSpace-=oneFileSize;
        contents+= " "  + oneFileSize;
        return true;
    }

    public String toString(){
        return id + "(" + remainingSpace + ")  :" + contents;
     }

}
