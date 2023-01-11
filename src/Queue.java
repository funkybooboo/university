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
        SIZE += 1;
    }

    public void remove(E data) {
        if (head != null) {
            if (head.data == data) head.next = head;
            else remove(data, head, head.next);
        }
    }

    private void remove(E data, Node<E> prevNode, Node<E> currentNode) {
        if (currentNode != null) {
            if (currentNode == tail && currentNode.data == data) {
                prevNode.next = null;
                prevNode = tail;

            } else {
                if (currentNode.data == data) prevNode.next = currentNode.next;
                else remove(data, prevNode.next, currentNode.next);
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
