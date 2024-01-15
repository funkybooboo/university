class Graph:
    def __init__(self, proposer_file_path, receiver_file_path):
        self.vertices = []
        self.adjacency_matrix = []
        self.vertex_count = 0
        self.edges = []
        self.residual = []
        self.cost_array = []
        self.proposer_file_path = proposer_file_path
        self.receiver_file_path = receiver_file_path
        self.create_graph()


    def create_graph(self):
        self.vertices.append("Source")  # create a list of all vertices
        self.read_preferences(self.proposer_file_path)
        proposer_count = len(self.vertices) - 1
        self.read_preferences(self.receiver_file_path)
        self.vertices.append("Sink")
        # Sort edges to get those with same to/from nodes together
        self.edges.sort()
        self.edges = self.combine_edges(self.edges)
        sink = len(self.vertices) - 1
        # set up edges as max flow problem
        for proposer in range(1, proposer_count + 1):
            self.edges.append((0, proposer, 0, 1))
        for receiver in range(proposer_count + 1, sink):
            self.edges.append((receiver, sink, 0, 1))
        self.make_adjacency()


    def read_preferences(self, file_path):
        with open(file_path) as file:
            for line in file:
                info = line.split(':')
                name = info[0].strip()
                self.vertices.append(name)
                if name:
                    priorities = info[1].strip().split(',')
                    for i in range(len(priorities)):
                        priorities[i] = priorities[i].strip()
                        # create an edge a->b with cost and flow
                        self.edges.append((name, priorities[i], i + 1, 1))


    # We should have two edges: one from a->b indicate preference for men and women.
    # If we don't have two edges, one partner found the other unacceptable, so we can ignore that combination
    def combine(self, edge1, edge2):
        if edge1[0] == edge2[0] and edge1[1] == edge2[1]:
            return self.vertices.index(edge1[0]), self.vertices.index(edge1[1]), edge1[2] + edge2[2], 1
        return None


    # for our matching, we consider the cost of an edge to be the sum of costs for each partner.
    # In this method, we combine individual edges creating a new set of edges.
    def combine_edges(self, edges):
        new_edges = []
        i = 0
        while i < len(edges):
            e = self.combine(edges[i], edges[i + 1])
            if e is not None:
                new_edges.append(e)
                i += 2
            else:
                i += 1  # skip the edge without a match
        return new_edges


    # from the list of edges, create an adjacency matrix, residual matrix, and cost_matrix
    def make_adjacency(self):
        self.vertex_count = len(self.vertices)
        self.adjacency_matrix = []
        while len(self.adjacency_matrix) < self.vertex_count:
            temp = [0 for _ in range(self.vertex_count)]
            self.adjacency_matrix.append(temp)
        self.cost_array = [list(row) for row in self.adjacency_matrix]  # careful to get a deep copy

        for edge in self.edges:
            i = int(edge[0])
            j = int(edge[1])

            if i >= self.vertex_count or j >= self.vertex_count or i < 0 or j < 0:
                print(f"Not a Proper Input in Edge {i},{j}")
            else:
                self.adjacency_matrix[i][j] = edge[3]
                self.cost_array[i][j] = edge[2]
                self.cost_array[j][i] = -edge[2]
            self.residual = [list(row) for row in self.adjacency_matrix]  # careful to get a deep copy


    # print 2 d array a with label
    @staticmethod
    def print2d_array(label, a):
        print(label)
        for i in range(len(a)):
            print("%3d:" % i, end=' ')
            for j in range(len(a[i])):
                print("%3d" % (a[i][j]), end=' ')
            print()


    def do_flow(self):
        print("Vertices are: ")
        print(self.vertices)
        print("Edges are: ")
        print(self.edges)
        self.print2d_array("adjacency", self.adjacency_matrix)
        self.print2d_array("residual", self.residual)
        self.print2d_array("cost", self.cost_array)
        self.BellmanFord(0, len(self.vertices) - 1)


    # utility function used to print the matrix dist with label
    def print_array(self, label, dist):
        print(label)
        for i in range(self.vertex_count):
            print("{0}\t\t{1}".format(i, dist[i]))


    # The main function that finds shortest distances from src to
    # all other vertices using Bellman-Ford algorithm. The function
    # also detects negative weight cycle
    # We are interested in the path from the src to the sink.
    # If we never make it to the sink, there is no flow
    # return true if there is flow from src to sink.
    def BellmanFord(self, source, sink):
        # Step 1: Initialize distances from src to all other vertices
        # as INFINITE
        INFINITE = float("inf")
        distances = [INFINITE for _ in range(self.vertex_count)]  # distances/cost to each node
        predecessors = [-1 for _ in range(self.vertex_count)]  # predecessor of each node
        distances[source] = 0
        # Step 2: Relax all edges |V| - 1 times. A simple shortest
        # path from src to any other vertex can have at-most |V| - 1
        # edges
        for _ in range(self.vertex_count - 1):
            for u in range(self.vertex_count):
                for v in range(self.vertex_count):
                    if self.residual[u][v] > 0 and distances[u] != INFINITE and distances[u] + self.cost_array[u][v] < distances[v]:
                        distances[v] = distances[u] + self.cost_array[u][v]
                        predecessors[v] = u
        self.print_array("Predecessor", predecessors)
        self.print_array("Cost", distances)
        return predecessors[sink] >= 0

