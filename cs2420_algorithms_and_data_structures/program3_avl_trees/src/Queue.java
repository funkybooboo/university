public class Queue<E> implements Store<E>{
    private static class Node<T> {
        T data;
        Node<T> next;
        public Node(T data) {
            this.data = data;
        }
    }

    private Node<E> head;
    private Node<E> tail;
    private int size = 0;

    public boolean isNotEmpty() {
        return getSize() > 0;
    }

    public void insert(E data) {
        if (tail == null) {
            tail = new Node<>(data);
            head = tail;
        } else {
            tail.next = new Node<>(data);
            tail = tail.next;
        }
        size++;
    }

    public int getSize() {
        return size;
    }

    public E deleteMin() {
        if (head == null) {
            if (tail != null) {
                head = tail;
                size--;
                return head.data;
            }
            return null;
        }
        Node<E> temp = head;

        head = head.next;
        size--;
        return temp.data;
    }

    @Override
    public void print(String label) {

    }

    public E removeAnyData(E data) {
        if (head != null) {
            if (head.data == data) {
                Node<E> temp = head;
                head = head.next;
                size--;
                return temp.data;
            }
            else {
                Node<E> prevNode = head;
                Node<E> currentNode = head.next;
                while(true) {
                    if (currentNode == null) return null;
                    if (currentNode.data == data) {
                        if (currentNode == tail) tail = prevNode;
                        prevNode.next = currentNode.next;
                        size--;
                        return currentNode.data;
                    } else {
                        prevNode = currentNode;
                        currentNode = currentNode.next;
                    }
                }
            }
        }
        return null;
    }

    public void printContents (){
        printContents(head);
        System.out.println();
    }

    private void printContents (Node<E> currentNode) {
        if (currentNode != null) {
            System.out.print(currentNode.data + " ");
            printContents(currentNode.next);
        }
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder();
        return toString(stringBuilder, head);
    }

    private String toString(StringBuilder stringBuilder, Node<E> currentNode) {
        if (currentNode != null) {
            stringBuilder.append(currentNode.data).append(" ");
            toString(stringBuilder, currentNode.next);
        }
        return stringBuilder.toString();
    }
}
