// ******************ERRORS********************************
// Throws UnderflowException as appropriate

import java.util.*;

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
     * O(n) with n being the number of nodes
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
     * This routine runs in O(n) with n being the number of nodes
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
     * The complexity of finding the deepest node is O(n) with n being the number of nodes
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
     * The complexity of finding the flip is O(n) with n being the number of nodes
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
     * The complexity of nodesInLevel is O(n) with n being the number of nodes
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


    // O(n) with n being the number of nodes
    public int getNumberOfNodes() {
        int numberOfNodes = 0;
        numberOfNodes = getNumberOfNodes(root, numberOfNodes);
        return numberOfNodes;
    }

    private int getNumberOfNodes(BinaryNode<E> currentNode, int numberOfNodes) {
        if (currentNode == null) return numberOfNodes;
        numberOfNodes = getNumberOfNodes(currentNode.left, numberOfNodes);
        numberOfNodes = getNumberOfNodes(currentNode.right, numberOfNodes);
        return ++numberOfNodes;
    }

    /**
     * Print all paths from root to leaves
     * The complexity of printAllPaths is O(nlogn) with n being the number of nodes
     */
    public void printAllPaths() {
        Object[] path = new Object[getNumberOfNodes()];
        printAllPaths(root, path, 0);
    }

    private void printAllPaths(BinaryNode<E> currentNode, Object[] path, int pathLength) {
        if (currentNode == null) return;
        path[pathLength] = currentNode.element;
        pathLength++;
        if (currentNode.left == null && currentNode.right == null) {
            for (int i = 0; i < pathLength; i++) {
                System.out.print(path[i] + " ");
            }
            System.out.println();
            return;
        }
        printAllPaths(currentNode.left, path, pathLength);
        printAllPaths(currentNode.right, path, pathLength);
    }

    /**
     * Counts all non-null binary search trees embedded in tree
     *  The complexity of countBST is O(n) with n being the number of nodes
     * @return Count of embedded binary search trees
     */
    public Integer countBST() {
        if (root == null) return 0;
        Set<E> set = new HashSet<>();
        countBST(set, root);
        for (E e : set) {
            System.out.print(e+" ");
        }
        return set.size();
    }

    private void countBST(Set<E> set, BinaryNode<E> node) {
        if (node == null) return;
        List<SubTree> list = new ArrayList<>();
        getNodeList(node, list);
        boolean isTree = false;
        for (int i = list.size()-1; i >= 0; i--) {
            SubTree subTree = list.get(i);
            if (subTree.isSubTree()) {
                isTree = true;
            } else {
                isTree = false;
                break;
            }
        }
        if (isTree) set.add(node.element);
        countBST(set, node.left);
        countBST(set, node.right);
    }

    private void getNodeList(BinaryNode<E> node, List<SubTree> list) {
        if (node == null) return;
        SubTree subTree = new SubTree(node);
        list.add(subTree);
        getNodeList(node.left, list);
        getNodeList(node.right, list);
    }

    private class SubTree {
        BinaryNode<E> node;

        public SubTree(BinaryNode<E> node) {
            this.node = node;
        }

        public boolean isSubTree() {
            if (node == null) return false;
            if (node.left == null && node.right == null) return true;
            if (node.right != null) {
                if (node.left == null && node.right.element.compareTo(node.element) > 0) return true;
            }
            if (node.left != null) {
                if (node.left.element.compareTo(node.element) < 0 && node.right == null) return true;
            }
            if (node.left != null && node.right != null) {
                return node.left.element.compareTo(node.element) < 0 && node.right.element.compareTo(node.element) > 0;
            }
            return false;
        }
    }

    /**
     * Insert into a bst tree; duplicates are allowed
     * The complexity of bstInsert depends on the tree.  If it is balanced the complexity is O(log n)
     * @param item the item to insert.
     */
    public void bstInsert(E item) {
        root = bstInsert(item, root);
    }

    /**
     * Internal method to insert into a subtree.
     * In tree is balanced, this routine runs in O(log n)
     * @param item the item to insert.
     * @param node the node that roots the subtree.
     * @return the new root of the subtree.
     */
    private BinaryNode<E> bstInsert(E item, BinaryNode<E> node) {
        if (node == null)
            return new BinaryNode<E>(item, null, null);
        int compareResult = item.compareTo(node.element);
        if (compareResult < 0) {
            node.left = bstInsert(item, node.left);
        } else {
            node.right = bstInsert(item, node.right);
        }
        return node;
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
     * @param item is item to search for.
     * @param node the node that roots the subtree.
     * @return node containing the matched item.
     */
    private boolean contains(E item, BinaryNode<E> node) {
        if (node == null)
            return false;
        int compareResult = item.compareTo(node.element);
        if (compareResult < 0)
            return contains(item, node.left);
        else if (compareResult > 0)
            return contains(item, node.right);
        else {
            return true;    // Match
        }
    }

    /**
     * Remove all paths from tree that sum to less than given value
     * @param sum: minimum path sum allowed in final tree
     * O(nlogn) with n being the number of nodes
     */
    public void pruneK(Integer sum) {
        Object[] path = new Object[getNumberOfNodes()];
        List<Object[]> goodList = new ArrayList<>();
        List<Object[]> badList = new ArrayList<>();
        pruneK(root, sum, 0, path, 0, goodList, badList);
        List<Object> goodNodes = new ArrayList<>();
        List<Object> badNodes = new ArrayList<>();
        getNodes(goodList, goodNodes);
        getNodes(badList, badNodes);
        for (Object i : goodNodes) {
            badNodes.removeIf(i::equals);
        }
        for (int i = badNodes.size()-1; i >= 0; i--) {
            remove((E) badNodes.get(i));
        }
    }

    private static void getNodes(List<Object[]> list, List<Object> nodes) {
        for (Object[] p : list) {
            for (Object o : p) {
                if (o != null && !nodes.contains(o)) {
                    nodes.add(o);
                }
            }
        }
    }

    private void pruneK(BinaryNode<E> currentNode, Integer sum, int pathSum, Object[] path, int pathLength, List<Object[]> goodList, List<Object[]> badList) {
        if (currentNode == null) return;
        path[pathLength] = currentNode.element;
        pathLength++;
        if (currentNode.left == null && currentNode.right == null) {
            for (int i = 0; i < pathLength; i++) {
                pathSum += (Integer) path[i];
            }
            Object[] temp = new Object[pathLength];
            System.arraycopy(path, 0, temp, 0, pathLength);
            if (pathSum < sum) {
                badList.add(temp);
            } else {
                goodList.add(temp);
            }
            return;
        }
        pruneK(currentNode.left, sum, pathSum, path, pathLength, goodList, badList);
        pruneK(currentNode.right, sum, pathSum, path, pathLength, goodList, badList);
    }

    // O(nlogn) with n being number of nodes
    private void remove(E item) {
        root = remove(item, root);
    }

    private BinaryNode<E> remove(E item, BinaryNode<E> currentNode) {
        if (currentNode == null) return null;
        int compareResult = item.compareTo(currentNode.element);
        if (compareResult == 0) {
            if (currentNode.left != null && currentNode.right != null) {
                currentNode = findMin(currentNode.right);
                if (currentNode != null) currentNode.right = remove(currentNode.element, currentNode.right);
            } else {
                if (currentNode.left != null) {
                    currentNode = currentNode.left;
                } else {
                    currentNode = currentNode.right;
                }
            }
        } else {
            currentNode.left = remove(item, currentNode.left);
            currentNode.right = remove(item, currentNode.right);
        }
        return currentNode;
    }

    private BinaryNode<E> findMin(BinaryNode<E> currentNode) {
        if (currentNode != null) {
            currentNode = currentNode.left;
        }
        return currentNode;
    }

    /**
     * Build tree given inOrder and preOrder traversals.  Each value is unique
     * @param inOrder  List of tree nodes in inorder
     * @param preOrder List of tree nodes in preorder
     * O(n) with n being the length of the given lists
     */
    public void buildTreeTraversals(Object[] inOrder, Object[] preOrder) {
        root = null;
        if (preOrder.length == 0 || inOrder.length == 0) return;
        root = buildTreeTraversals(inOrder, preOrder, root);
    }

    private BinaryNode<E> buildTreeTraversals(Object[] inOrder, Object[] preOrder, BinaryNode<E> node) {
        if (inOrder.length == 0 || preOrder.length == 0) return null;
        node = new BinaryNode<E>((E) preOrder[0]);
        int rootIndex = -1;
        for (int i = 0; i < inOrder.length; i++) {
            if (preOrder[0] == inOrder[i]) {
                rootIndex = i;
                break;
            }
        }
        if (rootIndex == -1) return null;

        Object[] leftSubInOrder = new Object[rootIndex];
        // everything less than rootIndex
        System.arraycopy(inOrder, 0, leftSubInOrder, 0, rootIndex);

        Object[] rightSubInOrder = new Object[inOrder.length-rootIndex-1];
        // everything greater than rootIndex
        System.arraycopy(inOrder, rootIndex+1, rightSubInOrder, 0, inOrder.length-rootIndex-1);

        Object[] leftSubPreOrder = new Object[leftSubInOrder.length];
        // items in the left sub in order list
        int index = 0;
        for (Object e : preOrder) {
            for (Object o : leftSubInOrder) {
                if (index >= leftSubPreOrder.length) break;
                if (e == o) {
                    leftSubPreOrder[index] = o;
                    index++;
                }
            }
        }

        Object[] rightSubPreOrder = new Object[rightSubInOrder.length];
        // items in the right sub in order list
        index = 0;
        for (Object e : preOrder) {
            for (Object o : rightSubInOrder) {
                if (index >= rightSubPreOrder.length) break;
                if (e == o) {
                    rightSubPreOrder[index] = o;
                    index++;
                }
            }
        }

        node.left = buildTreeTraversals(leftSubInOrder, leftSubPreOrder, node);
        node.right = buildTreeTraversals(rightSubInOrder, rightSubPreOrder, node);
        return node;
    }

    /**
     * Find the least common ancestor of two nodes
     * @param a first node
     * @param b second node
     * @return String representation of ancestor
     * O(logn) with n being the number of nodes
     */
    public String lca(E a, E b) {
        BinaryNode<E> ancestor;
        if (a.compareTo(b) < 0) {
            ancestor = lca(root, a, b);
        } else {
            ancestor = lca(root, b, a);
        }
        if (ancestor == null) return "none";
        else return ancestor.toString();
    }

    private BinaryNode<E> lca(BinaryNode<E> currentNode, E a, E b) {
        Object[] aPath = getPath(currentNode, a, new Object[getNumberOfNodes()], 0);
        Object[] bPath = getPath(currentNode, b, new Object[getNumberOfNodes()], 0);
        if (aPath == null || bPath == null) return null;
        List<Object> alist = trimArray(aPath);
        List<Object> blist = trimArray(bPath);
        for (int i = alist.size()-1; i >= 0; i--) {
            for (int j = blist.size()-1; j >= 0; j--) {
                if (alist.get(i) == blist.get(j)) {
                    return new BinaryNode<>((E)alist.get(i));
                }
            }
        }
        return null;
    }

    private static List<Object> trimArray(Object[] path) {
        List<Object> list = new ArrayList<>();
        for (Object o : path) {
            if (o != null) {
                list.add(o);
            }
        }
        return list;
    }

    private Object[] getPath(BinaryNode<E> currentNode, E n, Object[] path, int pathLength) {
        if (currentNode == null) return null;
        path[pathLength] = currentNode.element;
        pathLength++;
        Object[] pathToNode;
        if (currentNode.element == n) {
            pathToNode = path;
        } else {
            pathToNode = getPath(currentNode.left, n, path, pathLength);
            if (pathToNode == null) {
                pathToNode = getPath(currentNode.right, n, path, pathLength);
            }
        }
        return pathToNode;
    }

    /**
     * Balance the tree
     * O(nlogn) n being the number of nodoes in the tree
     */
    public void balanceTree() {
        List<BinaryNode<E>> nodeList = new ArrayList<>();
        getListOfNodes(nodeList, root);
        balanceTree(nodeList);
    }

    private void balanceTree(List<BinaryNode<E>> nodeList) {
        List<E> listOfRemovedNodes = new ArrayList<>();
        for (BinaryNode<E> node : nodeList) {
            remove(node.element);
            listOfRemovedNodes.add(node.element);
        }
        Collections.sort(listOfRemovedNodes);
        Object[] elements = listOfRemovedNodes.toArray();
        populateTree(elements, 0, elements.length-1);
    }

    private void populateTree(Object[] elements, int front, int end) {
        if (front > end) return;
        int middle = (front+end)/2;
        E middleItem = (E) elements[middle];
        bstInsert(middleItem);
        populateTree(elements, front, middle-1);
        populateTree(elements, middle+1, end);
    }

    private void getListOfNodes(List<BinaryNode<E>> nodeList, BinaryNode<E> node) {
        if (node == null) return;
        nodeList.add(node);
        getListOfNodes(nodeList, node.left);
        getListOfNodes(nodeList, node.right);
    }

    /**
     * In a BST, keep only nodes between range
     *
     * @param a lowest value
     * @param b highest value
     *  O(n) with n being the number of nodes
     */
    public void keepRange(E a, E b) {
        List<BinaryNode<E>> nodeList = new ArrayList<>();
        getListOfNodes(nodeList, root);
        for (int i = nodeList.size()-1; i >= 0; i--) {
            BinaryNode<E> n = nodeList.get(i);
            if (!(n.element.compareTo(a) >= 0) || !(n.element.compareTo(b) <= 0)) {
                remove(n.element);
            }
        }
    }

    // Basic node stored in unbalanced binary  trees

}
