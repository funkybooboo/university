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
                    if (residualCapacity[i][j] != 0 && costsFromSource[i] + edgeCosts[i][j] < costsFromSource[j]) {
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
        path.append(sink);
        int pred = sink;
        while (pred != source) {
            path.append(predecessors[pred]);
            pred = predecessors[pred];
        }
        return path.reverse().toString();
    }

    private int getFlow(String path) {
        int flow = 0;
        for (int i = 0, j = 1; i < path.length() && j < path.length(); i++, j++) {
            int a = Integer.parseInt(String.valueOf(path.charAt(i)));
            int b = Integer.parseInt(String.valueOf(path.charAt(j)));
            if (residualCapacity[a][b] > flow) {
                flow = residualCapacity[a][b];
            }
        }
        return flow;
    }

    private void updateResidual(String path) {
        for (int i = 0, j = 1; i < path.length() && j < path.length(); i++, j++) {
            int a = Integer.parseInt(String.valueOf(path.charAt(i)));
            int b = Integer.parseInt(String.valueOf(path.charAt(j)));
            int temp = residualCapacity[a][b];
            residualCapacity[a][b] = 0;
            residualCapacity[b][a] = -temp;
        }
    }

    private String getFinalPaths() {
        StringBuilder pathsToSink = new StringBuilder();
        for (int i = 0; i < vertexCount; i++) {
            StringBuilder path = new StringBuilder();
            path.append(i);
            for (int j = 0; j < vertexCount; j++) {
                if (originalCapacity[i][j] == 1 && residualCapacity[i][j] == 0) {
                    path.append(j);

                }
            }
            pathsToSink.append(path).append("\n");
        }

        return pathsToSink.toString();
    }

    private void findWeightedFlow() {
        System.out.println("working on getting paths");
        while(bellmanFord()) {
            String path = getSourceToSinkPath();
            int flow = getFlow(path);
            System.out.println("Found flow and path: "+flow+" "+path);
            updateResidual(path);
        }
    }

    private void finalEdgeFlow() {
        System.out.println("Final paths");
        int totalFlow = 0;
        String[] paths = getFinalPaths().split("\n");
        for (String path : paths) {
            int flow = getFlow(path);
            System.out.println("flow and path: "+flow+" "+path);
            totalFlow += flow;
        }
        System.out.println("total flow: "+totalFlow);
    }

    public void minCostMaxFlow(){
        System.out.println(printMatrix("Capacity", originalCapacity));
        findWeightedFlow();
        System.out.println(printMatrix("Residual", residualCapacity));
        finalEdgeFlow();
    }

    public static void main(String[] args) {
        // testing
        Graph graph = new Graph("data/match0.txt");
        graph.minCostMaxFlow();
    }

//    public static void main(String[] args) {
//        String[] files = {"data/match0.txt", "data/match1.txt", "data/match2.txt", "data/match3.txt", "data/match4.txt", "data/match5.txt","data/match6.txt", "data/match7.txt", "data/match8.txt", "data/match9.txt"};
//        for (String fileName : files) {
//            Graph graph = new Graph(fileName);
//            graph.minCostMaxFlow();
//        }
//    }
}