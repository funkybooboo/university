class Graph:
    def __init__(self, proposer_file_path, receiver_file_path):
        self.vertices = []
        self.adjacency_matrix = []
        self.vertex_count = 0
        self.edges = []
        self.residual_matrix = []
        self.cost_matrix = []
        self.proposer_file_path = proposer_file_path
        self.receiver_file_path = receiver_file_path
        self.create_graph()


    def create_graph(self):
        self.vertices.append("Source")  # create a list of all vertices
        self.read_preferences(self.proposer_file_path, "proposer")
        proposer_count = len(self.vertices) - 1
        self.read_preferences(self.receiver_file_path, "receiver")
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


    def add_proposer_edge(self, name, priorities, i):
        self.edges.append((name, priorities[i], i + 1, 1))

    def add_receiver_edge(self, name, priorities, i):
        self.edges.append((priorities[i], name, i + 1, 1))

    def read_preferences(self, file_path, type_):
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
                        if type_ == "proposer":
                            self.add_proposer_edge(name, priorities, i)
                        else:
                            self.add_receiver_edge(name, priorities, i)


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
        while i < len(edges) - 1:
            edge = self.combine(edges[i], edges[i + 1])
            if edge is not None:
                new_edges.append(edge)
                i += 2
            else:
                i += 1  # skip the edge without a match
        return new_edges


    # from the list of edges, create an adjacency matrix, residual matrix, and cost_matrix
    def make_adjacency(self):
        self.vertex_count = len(self.vertices)
        self.adjacency_matrix = []
        while len(self.adjacency_matrix) < self.vertex_count:
            self.adjacency_matrix.append([0 for _ in range(self.vertex_count)])

        self.cost_matrix = [list(row) for row in self.adjacency_matrix]  # careful to get a deep copy

        for edge in self.edges:
            u = int(edge[0])
            v = int(edge[1])

            if u >= self.vertex_count or v >= self.vertex_count or u < 0 or v < 0:
                print(f"Not a Proper Input in Edge {u},{v}")
            else:
                self.adjacency_matrix[u][v] = edge[3]
                self.cost_matrix[u][v] = edge[2]
                self.cost_matrix[v][u] = -edge[2]
            self.residual_matrix = [list(row) for row in self.adjacency_matrix]  # careful to get a deep copy


    def print_array(self, label, array):
        print(label)
        for i in range(self.vertex_count):
            print("{0}\t\t{1}".format(i, array[i]))


    @staticmethod
    def print_matrix(label, matrix):
        print()
        max_len = max(len(str(item)) for row in matrix for item in row) + 2
        delimiter = f"%{max_len}d"
        print(label)
        print("        ", end='')
        number_of_spaces = 0
        for i in range(len(matrix)):
            print(delimiter % i, end=' ')
            number_of_spaces += 1
        print("\n", "        ", end='')
        print("- " * number_of_spaces * max_len)
        for i in range(len(matrix)):
            print(f"{delimiter} |  " % i, end=' ')
            for j in range(len(matrix[i])):
                print(delimiter % (matrix[i][j]), end=' ')
            print()


    def do_flow(self):
        print("Vertices are: ")
        print(self.vertices)
        print("Edges are: ")
        print(self.edges)
        self.print_matrix("adjacency", self.adjacency_matrix)
        self.print_matrix("residual", self.residual_matrix)
        self.print_matrix("cost", self.cost_matrix)
        self.ford_fulkerson(0, len(self.vertices) - 1)


    def ford_fulkerson(self, source, sink):
        # 1 find an augmenting paths using bellman_ford
        # 2 go though the residual array and augment the flow (updating the flow of the edges along the augmenting path
        # amd decrease back edge flow) until no more augmenting paths can be found
        # the flow graph = the residual edges
        # e.capacity - e.flow = flow left
        flow_count = 0
        while True:
            is_valid, predecessors, costs = self.bellman_ford(source, sink)
            if not is_valid:
                break
            augmenting_path = self.get_augmenting_path(source, sink, predecessors)
            print()
            print("path: ", augmenting_path)
            self.augment_flow(augmenting_path)
            self.print_matrix("residual", self.residual_matrix)
            flow_count += 1
        print("flow count: ", flow_count)


    def augment_flow(self, augmenting_path):
        i = 0
        j = i + 1
        while j < len(augmenting_path):
            u = augmenting_path[i]
            v = augmenting_path[j]
            self.residual_matrix[v][u] = -self.residual_matrix[u][v]
            self.residual_matrix[u][v] = 0
            i += 1
            j = i + 1


    @staticmethod
    def get_augmenting_path(source, sink, predecessors):
        augmenting_path = [sink]
        v = sink
        while v > source:
            augmenting_path.append(predecessors[v])
            v = predecessors[v]
        augmenting_path.reverse()
        return augmenting_path


    @staticmethod
    def get_smallest_cost_for_path(costs, augmenting_path):
        smallest_cost = costs[augmenting_path[0]]
        for i in range(1, len(augmenting_path)):
            if smallest_cost > costs[augmenting_path[i]]:
                smallest_cost = costs[augmenting_path[i]]
        return smallest_cost


    # The main function that finds shortest distances from src to
    # all other vertices using Bellman-Ford algorithm. The function
    # also detects negative weight cycle
    # We are interested in the path from the src to the sink.
    # If we never make it to the sink, there is no flow
    # return true if there is flow from src to sink.
    def bellman_ford(self, source, sink):
        # Step 1: Initialize costs from src to all other vertices
        # as INFINITE
        INFINITE = float("inf")
        costs = [INFINITE for _ in range(self.vertex_count)]  # costs/cost to each node
        predecessors = [-1 for _ in range(self.vertex_count)]  # predecessor of each node
        costs[source] = 0
        # Step 2: Relax all edges |V| - 1 times. A simple shortest
        # path from src to any other vertex can have at-most |V| - 1
        # edges
        for _ in range(self.vertex_count - 1):
            for u in range(self.vertex_count):
                for v in range(self.vertex_count):
                    if self.residual_matrix[u][v] > 0 and costs[u] != INFINITE and costs[u] + self.cost_matrix[u][v] < costs[v]:
                        costs[v] = costs[u] + self.cost_matrix[u][v]
                        predecessors[v] = u
        self.print_array("Predecessor", predecessors)
        self.print_array("Cost", costs)
        return predecessors[sink] >= 0, predecessors, costs


