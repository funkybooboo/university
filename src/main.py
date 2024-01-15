"""
Written by Vicki Allan using Bellman Ford pseudocode
For me, it is easier to write the min cost max flow using adjacency matrices, so that is what this code does
Feel free to do it your way.
I was learning Python as I wrote, so I'm sure you'll shake your head at some of the things I did.  Shaking
your head is good exercise!
"""


from graph import Graph


def main():
    """
    files = [("men0.txt", "women0.txt", True), ("men.txt", "women.txt", True), ("men2.txt", "women2.txt", True),
     ("men3.txt", "women3.txt", False), ("men4.txt", "women4.txt", False)]
    """
    graph = Graph()
    graph.do_flow()


if __name__ == "__main__":
    main()