import java.io.File;
import java.util.*;

public class Graph {
    private int vertexCount;  // Number of vertices in the graph
    private int[][] originalCapacity;  // Adjacency  matrix | original copy into residual and move things around there
    private int[][] edgeCosts; // cost of edges in the matrix | set with costs then don't touch
    private int[][] residualCapacity; // residual matrix | where I currently am
    private final String graphName;  //The file from which the graph was created.
    private int source; // start of all paths
    private int sink; // end of all paths
    private int[] costsFromSource;
    private int[] predecessors;
    private final int INFINITY;

    public Graph(String fileName) {
        INFINITY = 999999;
        vertexCount = 0;
        source = 0;
        graphName = fileName;
        makeGraph();
    }

    private void makeGraph() {
        try {
            System.out.println("\n****Find Flow " + graphName);
            Scanner reader = new Scanner(new File(graphName));
            vertexCount = reader.nextInt();
            originalCapacity = new int[vertexCount][vertexCount];
            edgeCosts = new int[vertexCount][vertexCount];
            residualCapacity = new int[vertexCount][vertexCount];
            for (int i = 0; i < vertexCount; i++) {
                for (int j = 0; j < vertexCount; j++) {
                    originalCapacity[i][j] = 0;
                    edgeCosts[i][j] = 0;
                    residualCapacity[i][j] = 0;
                }
            }
            while (reader.hasNextInt()) {
                int v1 = reader.nextInt();
                int v2 = reader.nextInt();
                int capacity = reader.nextInt();
                int weight = reader.nextInt();
                if (!addEdge(v1, v2, capacity, weight))
                    throw new Exception();
            }
            reader.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
        sink = vertexCount - 1;
        predecessors = new int[vertexCount];
        costsFromSource = new int[vertexCount];
        System.out.println(printMatrix("Edge Cost" , edgeCosts));
    }

    private boolean addEdge(int source, int destination, int capacity, int weight) {
        if (source < 0 || source >= vertexCount) return false;
        if (destination < 0 || destination >= vertexCount) return false;
        this.originalCapacity[source][destination] = capacity;
        edgeCosts[source][destination] = weight;
        edgeCosts[destination][source] = -weight;
        residualCapacity[source][destination] = capacity;
        return true;
    }

    public String printMatrix(String label, int[][] matrix) {
        StringBuilder sb = new StringBuilder();
        sb.append("\n ").append(label).append(" \n     ");
        for (int i = 0; i < vertexCount; i++) {
            sb.append(String.format("%5d", i));
        }
        sb.append("\n");
        for (int i = 0; i < vertexCount; i++) {
            sb.append(String.format("%5d",i));
            for (int j = 0; j < vertexCount; j++) {
                sb.append(String.format("%5d",matrix[i][j]));
            }
            sb.append("\n");
        }
        return sb.toString();
    }

    private boolean bellmanFord() {
        resetCostsAndPredecessors();
        for (int repeat = 1; repeat < vertexCount; repeat++) {
            for (int i = 0; i < vertexCount; i++) {
                for (int j = 0; j < vertexCount; j++) {
                    if (residualCapacity[i][j] > 0 && costsFromSource[i] + edgeCosts[i][j] < costsFromSource[j]) {
                        costsFromSource[j] = costsFromSource[i] + edgeCosts[i][j];
                        predecessors[j] = i;
                    }
                }
            }
        }
        return predecessors[sink] != INFINITY;
    }

    private void resetCostsAndPredecessors() {
        for (int i = 0; i < vertexCount; i++) {
            costsFromSource[i] = INFINITY;
            predecessors[i] = INFINITY;
        }
        costsFromSource[source] = 0;
        predecessors[source] = source;
    }

    private String getSourceToSinkPath() {
        StringBuilder path = new StringBuilder();
        path.append(sink).append(" ");
        int pred = sink;
        while (pred != source) {
            path.append(predecessors[pred]).append(" ");
            pred = predecessors[pred];
        }
        reverse(path);
        return path.toString().trim();
    }

    private void reverse(StringBuilder path) {
        String[] splitPath = path.toString().trim().split(" ");
        path.setLength(0);
        for (int i = splitPath.length-1; i >= 0; i--) {
            path.append(splitPath[i]).append(" ");
        }
    }

    private int getFlow(String path, int[][] array) {
        String[] splitPath = path.split(" ");
        int flow = INFINITY;
        for (int i = 0, j = 1; i < splitPath.length && j < splitPath.length; i++, j++) {
            int a = Integer.parseInt(splitPath[i]);
            int b = Integer.parseInt(splitPath[j]);
            if (array[a][b] < flow) {
                flow = array[a][b];
            }
        }
        return flow;
    }

    private int getPathCost(String path) {
        String[] splitPath = path.split(" ");
        int cost = 0;
        for (int i = 0, j = 1; i < splitPath.length && j < splitPath.length; i++, j++) {
            int a = Integer.parseInt(splitPath[i]);
            int b = Integer.parseInt(splitPath[j]);
            cost += edgeCosts[a][b];
        }
        return cost;
    }

    private void updateMatrix(int flow, String path, int[][] array) {
        String[] splitPath = path.split(" ");
        for (int i = 0, j = 1; i < splitPath.length && j < splitPath.length; i++, j++) {
            int a = Integer.parseInt(splitPath[i]);
            int b = Integer.parseInt(splitPath[j]);
            array[a][b] = array[a][b] - flow;
            array[b][a] = array[b][a] + flow;
        }
    }

    // This method is unneeded for assignment, but it gets final paths after bellmanFord is done.
    private String getFinalPaths() {
        StringBuilder paths = new StringBuilder();
        int[][] tempResidual = residualCapacity.clone();
        for (int col = 0; col < vertexCount; col++) {
            StringBuilder path = new StringBuilder();
            if (originalCapacity[source][col] > 0 && tempResidual[source][col] != 0) {
                path.append(source).append(" ");
                path.append(col).append(" ");
                int next = col;
                do {
                    next = buildPath(next, tempResidual);
                    if (next > 0) {
                        path.append(next).append(" ");
                    }
                } while (next != sink && next != -1);
                if (next != -1) {
                    paths.append(path.toString().trim()).append("\n");
                }
                int flow = getFlow(path.toString(), tempResidual);
                updateMatrix(flow, path.toString(), tempResidual);
            }
        }
        return paths.toString();
    }
    private int buildPath(int row, int[][] tempResidual) {
        for (int col = 0; col < vertexCount; col++) {
            if (originalCapacity[row][col] > 0 && tempResidual[source][col] != 0) {
                return col;
            }
        }
        return -1;
    }

    private void findWeightedFlow() {
        System.out.println("Paths found in order");
        while(bellmanFord()) {
            String path = getSourceToSinkPath();
            int flow = getFlow(path, residualCapacity);
            int pathCost = getPathCost(path);
            System.out.println("Flow "+path+" ("+flow+")"+" $"+pathCost);
            updateMatrix(flow, path, residualCapacity);
        }
    }

    private void finalEdgeFlow() {
        System.out.println("Final flow on each edge");
        for (int i = 1; i < vertexCount-1; i++) {
            for (int j = 1; j < vertexCount-1; j++) {
                if (originalCapacity[i][j] > 0 && residualCapacity[i][j] < originalCapacity[i][j]) {
                    int flow = originalCapacity[i][j] - residualCapacity[i][j];
                    System.out.println("Flow "+i+"->"+j+" ("+flow+") $"+edgeCosts[i][j]);
                }
            }
        }
    }

    public void minCostMaxFlow(){
        System.out.println(printMatrix("Capacity", originalCapacity));
        findWeightedFlow();
        System.out.println(printMatrix("Residual", residualCapacity));
        finalEdgeFlow();
    }

    public static void main(String[] args) {
        String[] files = {"data/match0.txt", "data/match1.txt", "data/match2.txt", "data/match3.txt", "data/match4.txt", "data/match5.txt","data/match6.txt", "data/match7.txt", "data/match8.txt", "data/match9.txt"};
        for (String fileName : files) {
            Graph graph = new Graph(fileName);
            graph.minCostMaxFlow();
        }
    }
}