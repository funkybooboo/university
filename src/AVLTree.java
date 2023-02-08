// AvlTree class
//
// CONSTRUCTION: with no initializer
//
// ******************PUBLIC OPERATIONS*********************
// void insert( x )       --> Insert x
// void remove( x )       --> Remove x (unimplemented)
// boolean contains( x )  --> Return true if x is present
// boolean remove( x )    --> Return true if x was present
// Comparable findMin( )  --> Return smallest item
// Comparable findMax( )  --> Return largest item
// boolean isEmpty( )     --> Return true if empty; else false
// void makeEmpty( )      --> Remove all items
// void printTree( )      --> Print tree in sorted order
// ******************ERRORS********************************
// Throws UnderflowException as appropriate

/**
 * Implements an AVL tree.
 * Note that all "matching" is based on the compareTo method.
 * @author Mark Allen Weiss
 */
public class AVLTree<E extends Comparable<? super E>> {

    private static class Node<T> {
        T data;
        Node<T> left;
        Node<T> right;
        int height;

        Node(T data) {
            this(data, null, null );
        }

        Node(T data, Node<T> left, Node<T> right) {
            this.data = data;
            this.left = left;
            this.right = right;
            this.height = 0;
        }
    }

    /** The tree root. */
    private Node<E> root;

    /**
     * Construct the tree.
     */
    public AVLTree() {
        root = null;
    }

    /**
     * Insert into the tree.
     * @param data the item to insert.
     */
    public void insert(E data) {
        root = insert( data, root );
    }

    /**
     * Remove from the tree. Nothing is done if data is not found.
     * @param data the item to remove.
     */
    public void remove(E data) {
        root = remove( data, root );
    }

    /**
     * Internal method to remove from a subtree.
     * @param data the item to remove.
     * @param node the node that roots the subtree.
     * @return the new root of the subtree.
     */
    private Node<E> remove(E data, Node<E> node) {
        if( node == null ) return node;
        int compareResult = data.compareTo(node.data);
        if( compareResult < 0 ) {
            node.left = remove(data, node.left);
        } else if(compareResult > 0) {
            node.right = remove(data, node.right);
        } else if(node.left != null && node.right != null) {
            node.data = findMin(node.right).data;
            node.right = remove(node.data, node.right);
        }
        else {
            node = (node.left != null) ? node.left : node.right;
        }
        return balance(node);
    }

    /**
     * Find the smallest item in the tree.
     * @return smallest item or null if empty.
     */
    public E findMin() {
        if(isEmpty()) throw new RuntimeException();
        return findMin(root).data;
    }



    /**
     * Find the largest item in the tree.
     * @return the largest item of null if empty.
     */
    public E findMax() {
        if(isEmpty()) throw new RuntimeException();
        return findMax(root).data;
    }

    /**
     * Find an item in the tree.
     * @param item the item to search for.
     * @return true if item is found.
     */
    public boolean contains(E item) {
        return contains(item, root);
    }

    /**
     * Make the tree logically empty.
     */
    public void makeEmpty() {
        root = null;
    }

    /**
     * Test if the tree is logically empty.
     * @return true if empty, false otherwise.
     */
    public boolean isEmpty() {
        return root == null;
    }

    /**
     * Print the tree contents in sorted order.
     */
    public void printTree(String label) {
        System.out.println(label);
        if(isEmpty()) {
            System.out.println("Empty tree");
        }
        else {
            printTree(root,"");
        }
    }

    private static final int ALLOWED_IMBALANCE = 1;

    // Assume node is either balanced or within one of being balanced
    private Node<E> balance(Node<E> node) {
        if(node == null) return node;
        if(height(node.left) - height(node.right) > ALLOWED_IMBALANCE) {
            if(height(node.left.left) >= height(node.left.right)) {
                node = rightRotation( node );
            }
            else {
                node = doubleRightRotation( node );
            }
        } else {
            if(height(node.right) - height(node.left) > ALLOWED_IMBALANCE) {
                if(height(node.right.right) >= height(node.right.left)) {
                    node = leftRotation(node);
                }
                else {
                    node = doubleLeftRotation(node);
                }
            }
        }
        node.height = Math.max(height(node.left), height(node.right)) + 1;
        return node;
    }

    public void checkBalance() {
        checkBalance(root);
    }

    private int checkBalance(Node<E> node) {
        if(node == null) return -1;
        int hl = checkBalance(node.left);
        int hr = checkBalance(node.right);
        if(Math.abs( height(node.left) - height(node.right)) > 1 || height(node.left) != hl || height(node.right) != hr) {
            System.out.println("\n\n***********************OOPS!!");
        }
        return height(node);
    }

    /**
     * Internal method to insert into a subtree.  Duplicates are allowed
     * @param item the item to insert.
     * @param node the node that roots the subtree.
     * @return the new root of the subtree.
     */
    private Node<E> insert(E item, Node<E> node) {
        if(node == null) {
            return new Node<>(item, null, null);
        }
        int compareResult = item.compareTo(node.data);
        if(compareResult < 0){
            node.left = insert(item, node.left);
        }
        else{
            node.right = insert(item, node.right);
        }
        return balance(node);
    }

    /**
     * Internal method to find the smallest item in a subtree.
     * @param node the node that roots the tree.
     * @return node containing the smallest item.
     */
    private Node<E> findMin(Node<E> node) {
        if(node == null) return node;
        while(node.left != null) {
            node = node.left;
        }
        return node;
    }

    public E deleteMin() {
        Pair<Node<E>> pair = deleteMin(root);
        root = pair.data1;
        return pair.data2.data;
    }

    /**
     * returns the new tree after the smallest node has been
     * deleted from the subtree rooted at node
     */
   private Pair<Node<E>> deleteMin(Node<E> node) {
       if (node == null) return null;
       Node<E> minParent = node;
       Pair<Node<E>> pair = new Pair<>();
       pair.data1 = node;
       pair.data2 = minParent;
       if (minParent.left == null) return pair;

       Node<E> min = minParent.left;
       while(min != null) {
           minParent = minParent.left;
           min = min.left;
       }
       minParent.left = null;
       balance(minParent);
       pair.data1 = node;
       pair.data2 = min;
       return pair;
    }

    private static class Pair<T> {
       T data1;
       T data2;
    }

    /**
     * Internal method to find the largest item in a subtree.
     * @param node the node that roots the tree.
     * @return node containing the largest item.
     */
    private Node<E> findMax(Node<E> node) {
        if(node == null)
            return node;
        while(node.right != null)
            node = node.right;
        return node;
    }

    /**
     * Internal method to find an item in a subtree.
     * @param item is item to search for.
     * @param node the node that roots the tree.
     * @return true if x is found in subtree.
     */
    private boolean contains(E item, Node<E> node) {
        while(node != null) {
            int compareResult = item.compareTo(node.data);
            if(compareResult < 0) {
                node = node.left;
            }
            else if(compareResult > 0) {
                node = node.right;
            }
            else return true;    // Match
        }
        return false;   // No match
    }

    /**
     * Internal method to print a subtree in sorted order.
     * @param node the node that roots the tree.
     */
    private void printTree(Node<E> node, String indent) {
        if(node != null) {
            printTree(node.right, indent+"   ");
            System.out.println(indent + node.data + "("+ node.height  +")");
            printTree(node.left, indent+"   ");
        }
    }

    /**
     * Return the height of node node, or -1, if null.
     */
    private int height(Node<E> node) {
        if (node==null) return -1;
        return node.height;
    }

    /**
     * Rotate binary tree node with left child.
     * For AVL trees, this is a single rotation for case 1.
     * Update heights, then return new root.
     */
    private Node<E> rightRotation(Node<E> node) {
        Node<E> theLeft = node.left;
        node.left = theLeft.right;
        theLeft.right = node;
        node.height = Math.max( height(node.left), height( node.right)) + 1;
        theLeft.height = Math.max(height(theLeft.left), node.height) + 1;
        return theLeft;
    }

    /**
     * Rotate binary tree node with right child.
     * For AVL trees, this is a single rotation for case 4.
     * Update heights, then return new root.
     */
    private Node<E> leftRotation(Node<E> node) {
        Node<E> theRight = node.right;
        node.right = theRight.left;
        theRight.left = node;
        node.height = Math.max(height(node.left), height(node.right)) + 1;
        theRight.height = Math.max(height(theRight.right), node.height) + 1;
        return theRight;
    }

    /**
     * Double rotate binary tree node: first left child
     * with its right child; then node k3 with new left child.
     * For AVL trees, this is a double rotation for case 2.
     * Update heights, then return new root.
     */
    private Node<E> doubleRightRotation(Node<E> node) {
        node.left = leftRotation(node.left);
        return rightRotation(node);

    }

    /**
     * Double rotate binary tree node: first right child
     * with its left child; then node k1 with new right child.
     * For AVL trees, this is a double rotation for case 3.
     * Update heights, then return new root.
     */
    private Node<E> doubleLeftRotation(Node<E> node) {
        node.right = rightRotation(node.right);
        return leftRotation(node);
    }

    // Test program
    public static void main(String[] args) {
        AVLTree<Integer> tree1 = new AVLTree<>();
        AVLTree<Dwarf> tree2 = new AVLTree<>();

        String[] nameList = {"Snowflake", "Sneezy", "Doc", "Grumpy", "Bashful", "Dopey", "Happy", "Doc", "Grumpy", "Bashful", "Doc", "Grumpy", "Bashful"};
        for (String s : nameList) {
            tree2.insert(new Dwarf(s));
        }

        tree2.printTree("The Tree");

        tree2.remove(new Dwarf("Bashful"));

        tree2.printTree( "The Tree after delete Bashful" );

        for (int i = 0; i < 8; i++) {
            tree2.deleteMin();
            tree2.printTree("\n\n The Tree after deleteMin");
        }

    }

}
