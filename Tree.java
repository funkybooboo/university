// ******************ERRORS********************************
// Throws UnderflowException as appropriate

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class UnderflowException extends RuntimeException {
    /**
     * Construct this exception object.
     *
     * @param message the error message.
     */
    public UnderflowException(String message) {
        super(message);
    }
}

public class Tree<E extends Comparable<? super E>> {
    private static class BinaryNode<E> {
        E element;            // The data in the node
        BinaryNode<E> left;   // Left child
        BinaryNode<E> right;  // Right child

        // Constructors
        BinaryNode(E theElement) {
            this(theElement, null, null);
        }

        BinaryNode(E theElement, BinaryNode<E> lt, BinaryNode<E> rt) {
            element = theElement;
            left = lt;
            right = rt;
        }

        // toString for BinaryNode
        public String toString() {
            return "Node:" +
                    element;
        }
    }

    private BinaryNode<E> root;  // Root of tree
    private String treeName;     // Name of tree

    private int numberOfNodes;

    /**
     * Create an empty tree
     * @param label Name of tree
     */
    public Tree(String label) {
        treeName = label;
        root = null;
    }

    /**
     * Create tree from list
     * @param arr   List of elements
     * @param label Name of tree
     * @ordered true if you want an ordered tree
     */
    public Tree(E[] arr, String label, boolean ordered) {
        treeName = label;
        if (ordered) {
            root = null;
            for (E e : arr) {
                bstInsert(e);
            }
        } else root = buildUnordered(arr, 0, arr.length - 1);
    }

    /**
     * Build a NON BST tree by inorder
     * @param arr nodes to be added
     * @return new tree
     */
    private BinaryNode<E> buildUnordered(E[] arr, int low, int high) {
        if (low > high) return null;
        int mid = (low + high) / 2;
        BinaryNode<E> curr = new BinaryNode<>(arr[mid], null, null);
        curr.left = buildUnordered(arr, low, mid - 1);
        curr.right = buildUnordered(arr, mid + 1, high);
        return curr;
    }


    /**
     * Change name of tree
     * @param name new name of tree
     */
    public void changeName(String name) {
        this.treeName = name;
    }

    /**
     * Return a string displaying the tree contents as a single line
     */
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder();
        if (root == null) {
            return treeName + " Empty tree";
        } else {
            StringBuilder stringBuilder1 = new StringBuilder();
            return stringBuilder.append(treeName).append("\n").append(toString(root, stringBuilder1, 1)).toString();
        }
    }

    private StringBuilder toString(BinaryNode<E> currentNode, StringBuilder stringBuilder, int depth) {
        if (currentNode == null) return stringBuilder;
        int spacing = 5;
        toString(currentNode.right, stringBuilder, depth + spacing);
        stringBuilder.append(String.format("%" + depth + "d\n", currentNode.element));
        toString(currentNode.left, stringBuilder, depth + spacing);
        return stringBuilder;
    }

    /**
     * Return a string displaying the tree contents as a single line
     */
    public String toString2() {
        if (root == null)
            return treeName + " Empty tree";
        else
            return treeName + " " + toString2(root);
    }

    /**
     * Internal method to return a string of items in the tree in order
     * This routine runs in O(??)
     *
     * @param t the node that roots the subtree.
     */
    private String toString2(BinaryNode<E> t) {
        if (t == null) return "";
        return toString2(t.left) +
                t.element.toString() + " " +
                toString2(t.right);
    }


    /**
     * The complexity of finding the deepest node is O(???)
     * @return
     */
    public E deepestNode() {
        ArrayList<DataStore<Integer, E>> map = new ArrayList<>();
        deepestNode(root, 0, map);
        ArrayList<E> topStuff = new ArrayList<>();
        int maxDepth = 0;
        if (map.isEmpty()) return null;
        for (DataStore<Integer, E> store : map) {
            if (store.data1 >= maxDepth){
                maxDepth = store.data1;
                topStuff.add(store.data2);
            }
        }
        return topStuff.get(topStuff.size()-1);

    }

    private void deepestNode(BinaryNode<E> currentNode, int depth, ArrayList<DataStore<Integer, E>> map) {
        if (currentNode == null) return;
        deepestNode(currentNode.left, depth + 1, map);
        deepestNode(currentNode.right, depth + 1, map);
        DataStore<Integer, E> store = new DataStore<>();
        store.data1 = depth;
        store.data2 = currentNode.element;
        map.add(store);
    }

    /**
     * The complexity of finding the flip is O(???)
     * reverse left and right children recursively
     */
    public void flip() {
        flip(root);
    }

    private void flip(BinaryNode<E> currentNode) {
        if (currentNode == null) return;
        BinaryNode<E> tempNode;
        tempNode = currentNode.left;
        currentNode.left = currentNode.right;
        currentNode.right = tempNode;
        flip(currentNode.left);
        flip(currentNode.right);
    }

    /**
     * Counts number of nodes in specified level
     * The complexity of nodesInLevel is O(???)
     * @param wantedLevel Level in tree, root is zero
     * @return count of number of nodes at specified level
     */
    public int nodesInLevel(int wantedLevel) {
        ArrayList<DataStore<Integer, E>> map = new ArrayList<>();
        nodesInLevel(root, 0, map);
        int count = 0;
        for (DataStore<Integer, E> store : map) {
            if (store.data1 == wantedLevel) {
                count++;
            }
        }
        return count;
    }

    private void nodesInLevel(BinaryNode<E> currentNode, int level, ArrayList<DataStore<Integer, E>> map) {
        if (currentNode == null) return;
        nodesInLevel(currentNode.left, level + 1, map);
        nodesInLevel(currentNode.right, level + 1, map);
        DataStore<Integer, E> store = new DataStore<>();
        store.data1 = level;
        store.data2 = currentNode.element;
        map.add(store);
    }

    private static class DataStore<E, T> {
        E data1;
        T data2;
    }

    public int getNumberOfNodes() {
        numberOfNodes = 0;
        getNumberOfNodes(root);
        return numberOfNodes;
    }

    private void getNumberOfNodes(BinaryNode<E> currentNode) {
        if (currentNode == null) return;
        getNumberOfNodes(currentNode.left);
        getNumberOfNodes(currentNode.right);
        numberOfNodes++;
    }

    /**
     * Print all paths from root to leaves
     * The complexity of printAllPaths is O(???)
     */
    public void printAllPaths() {
        E[] path = (E[]) new Comparable[getNumberOfNodes()+100];
        printAllPaths(root, path, 0);
    }

    private void printAllPaths(BinaryNode<E> currentNode, E[] path, int pathLength) {
        if (currentNode == null) return;
        path[pathLength] = currentNode.element;
        pathLength++;
        if (currentNode.left == null && currentNode.right == null) {
            for (int i = 0; i < pathLength; i++) System.out.print(path[i] + " ");
            System.out.println();
            return;
        }
        printAllPaths(currentNode.left, path, pathLength);
        printAllPaths(currentNode.right, path, pathLength);
    }

    /**
     * Counts all non-null binary search trees embedded in tree
     *  The complexity of countBST is O(???)
     * @return Count of embedded binary search trees
     */
    public Integer countBST() {
        if (root == null) return 0;
        return -1;
    }

    /**
     * Insert into a bst tree; duplicates are allowed
     * The complexity of bstInsert depends on the tree.  If it is balanced the complexity is O(log n)
     * @param x the item to insert.
     */
    public void bstInsert(E x) {

        root = bstInsert(x, root);
    }

    /**
     * Internal method to insert into a subtree.
     * In tree is balanced, this routine runs in O(log n)
     * @param x the item to insert.
     * @param t the node that roots the subtree.
     * @return the new root of the subtree.
     */
    private BinaryNode<E> bstInsert(E x, BinaryNode<E> t) {
        if (t == null)
            return new BinaryNode<E>(x, null, null);
        int compareResult = x.compareTo(t.element);
        if (compareResult < 0) {
            t.left = bstInsert(x, t.left);
        } else {
            t.right = bstInsert(x, t.right);
        }
        return t;
    }

    /**
     * Determines if item is in tree
     * @param item the item to search for.
     * @return true if found.
     */
    public boolean contains(E item) {
        return contains(item, root);
    }

    /**
     * Internal method to find an item in a subtree.
     * This routine runs in O(log n) as there is only one recursive call that is executed and the work
     * associated with a single call is independent of the size of the tree: a=1, b=2, k=0
     *
     * @param x is item to search for.
     * @param t the node that roots the subtree.
     * @return node containing the matched item.
     */
    private boolean contains(E x, BinaryNode<E> t) {
        if (t == null)
            return false;

        int compareResult = x.compareTo(t.element);

        if (compareResult < 0)
            return contains(x, t.left);
        else if (compareResult > 0)
            return contains(x, t.right);
        else {
            return true;    // Match
        }
    }
    /**
     * Remove all paths from tree that sum to less than given value
     * @param sum: minimum path sum allowed in final tree
     */
    public void pruneK(Integer sum) {

    }

    /**
     * Build tree given inOrder and preOrder traversals.  Each value is unique
     * @param inOrder  List of tree nodes in inorder
     * @param preOrder List of tree nodes in preorder
     */
    public void buildTreeTraversals(E[] inOrder, E[] preOrder) {
        root = null;
    }

    /**
     * Find the least common ancestor of two nodes
     * @param a first node
     * @param b second node
     * @return String representation of ancestor
     */
    public String lca(E a, E b) {
        BinaryNode<E> ancestor = null;
//        if (a.compareTo(b) < 0) {
//            ancestor = lca(root, a, b);
//        } else {
//            ancestor = lca(root, b, a);
//        }
        if (ancestor == null) return "none";
        else return ancestor.toString();
    }

    /**
     * Balance the tree
     */
    public void balanceTree() {
        //root = balanceTree(root);
    }

    /**
     * In a BST, keep only nodes between range
     *
     * @param a lowest value
     * @param b highest value
     */
    public void keepRange(E a, E b) {
    }

    // Basic node stored in unbalanced binary  trees


}
