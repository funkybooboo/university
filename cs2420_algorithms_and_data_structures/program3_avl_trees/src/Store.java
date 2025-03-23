public interface Store<E> {
    boolean isNotEmpty();
    void insert(E e);
    E deleteMin();
    void print(String label);
}
