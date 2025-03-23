public class BinarySearchTree<E extends Comparable<E>> {
    private class Node<T extends Comparable<E>> {
        E data;
        Node<T> left;
        Node<T> right;
        public Node(E data) {
            this.data = data;
            numberOfNodes++;
        }
    }

    private Node<E> root;

    private int numberOfNodes;
    public int numberNodes() {
        return numberOfNodes;
    }

    public void display(String message) {
        System.out.println(message);
        display(root);
    }
    private void display(Node<E> node) {
        if(node == null) return;
        display(node.left);
        System.out.println(node.data);
        display(node.right);
    }

    public void displayPreOrder() {
        displayPreOrder(root);
    }
    private void displayPreOrder(Node<E> node) {
        if (node == null) return;
        System.out.println(node.data);
        displayPostOrder(node.left);
        displayPostOrder(node.right);
    }

    public void displayPostOrder() {
        displayPostOrder(root);
    }
    private void displayPostOrder(Node<E> node) {
        if (node == null) return;
        displayPostOrder(node.left);
        displayPostOrder(node.right);
        System.out.println(node.data);
    }

    public void displayVisualTree() {
        displayVisualTree(root, 1);
    }
    private void displayVisualTree(Node<E> node, int depth) {
        if (node == null) return;
        displayVisualTree(node.right, depth + 2);
        System.out.printf("%" + depth + "d\n", node.data);
        displayVisualTree(node.left, depth + 2);
    }

    public void displayInOrder() {
        displayInOrder(root);
    }
    private void displayInOrder(Node<E> node) {
        if (node == null) return;
        displayInOrder(node.left);
        System.out.println(node.data);
        displayInOrder(node.right);
    }

    public int numberLeafNodes() {
        return getLeafCount(root);
    }
    private int getLeafCount(Node<E> node) {
        if (node == null) return 0;
        if (node.left == null && node.right == null) return 1;
        else return getLeafCount(node.left) + getLeafCount(node.right);
    }

    public int height() {
        return height(root);
    }
    private int height(Node<E> node) {
        if (node == null) return -1;
        else {
            int leftDepth = height(node.left);
            int rightDepth = height(node.right);
            if (leftDepth > rightDepth) {
                return (leftDepth + 1);
            } else {
                return (rightDepth + 1);
            }
        }
    }

    public boolean search(E data) {
        Node<E> node = root;
        while (node != null) {
            if (node.data.equals(data)) {
                return true;
            } else if (node.data.compareTo(data) < 0) {
                node = node.right;
            } else {
                node = node.left;
            }
        }
        return false;
    }

    public boolean insert(E data) {
        boolean inserted = false;
        if (root == null) {
            root = new Node<>(data);
            inserted = true;
        } else {
            Node<E> parent = null;
            Node<E> node = root;
            while (node != null) {
                parent = node;
                if (node.data.compareTo(data) < 0) {
                    node = node.right;
                    inserted = true;
                }else if (node.data.compareTo(data) > 0){
                    node = node.left;
                    inserted = true;
                }else{
                    return false;
                }
            }
            Node<E> newNode = new Node<>(data);
            if (parent.data.compareTo(data) < 0) {
                parent.right = newNode;
            } else if (parent.data.compareTo(data) != 0) {
                parent.left = newNode;
            }
        }
        return inserted;
    }

    public boolean remove(E data) {
        Node<E> parent = null;
        Node<E> node = root;
        boolean done = false;
        boolean removed;
        while (!done) {
            if (node == null) return false;
            if (node.data.compareTo(data) < 0) {
                parent = node;
                node = node.right;
            } else if (node.data.compareTo(data) > 0) {
                parent = node;
                node = node.left;
            } else {
                done = true;
            }
        }
        if (node.left == null) {
            if (parent == null) {
                root = node.right;
                removed = true;
                numberOfNodes--;
            } else {
                if (parent.data.compareTo(data) < 0) {
                    parent.right = node.right;
                } else {
                    parent.left = node.right;
                }
                removed = true;
                numberOfNodes--;
            }
        } else {
            Node<E> parentOfRight = node;
            Node<E> rightMost = node.left;
            while (rightMost.right != null) {
                parentOfRight = rightMost;
                rightMost = rightMost.right;
            }
            node.data = rightMost.data;
            removed = true;
            numberOfNodes--;
            if (parentOfRight.right == rightMost) {
                parentOfRight.right = rightMost.left;
            } else {
                parentOfRight.left = rightMost.left;
            }
        }
        return removed;
    }

}
