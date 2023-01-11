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
    private static int SIZE = 0;

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

    public static int getSIZE() {
        return SIZE;
    }

    public E removeFront() {
        if (head == null) {
            return null;
        }
        Node<E> temp = head;
        head = head.next;
        SIZE--;
        return temp.data;
    }

    public E removeAnyData(E data) {
        if (head != null) {
            if (head.data == data) {
                Node<E> temp = head;
                head = head.next;
                SIZE--;
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
                        SIZE--;
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
}
