public class Queue<E> {
    private static class Node<T> {
        T data;
        Node<T> next;
        public Node(T data) {
            this.data = data;
        }
    }

    private Node<E> head;
    private Node<E> tail;
    public static int SIZE = 0;

    public void add(E data) {
        if (tail == null) {
            tail = new Node<>(data);
            head = tail;
        } else {
            tail.next = new Node<>(data);
            tail = tail.next;
        }
        SIZE++;
    }

    public void remove(E data) {
        if (head != null) {
            if (head.data == data) {
                head = head.next;
                SIZE--;
            }
            else {
                Node<E> prevNode = head;
                Node<E> currentNode = head.next;
                while(true) {
                    if (currentNode == null) break;
                    if (currentNode.data == data) {
                        if (currentNode == tail) tail = prevNode;
                        prevNode.next = currentNode.next;
                        SIZE--;
                        break;
                    } else {
                        prevNode = currentNode;
                        currentNode = currentNode.next;
                    }
                }
            }
        }
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
}
