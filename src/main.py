"""
Written by Vicki Allan using Bellman Ford pseudocode
For me, it is easier to write the min cost max flow using adjacency matrices, so that is what this code does
Feel free to do it your way.
I was learning Python as I wrote, so I'm sure you'll shake your head at some of the things I did.  Shaking
your head is good exercise!
"""


from graph import Graph


def main():
    print()

    print("Scenario 1")
    print("Employers -> Applicants")
    graph = Graph("../data/scenario1/Employers.txt", "../data/scenario1/Applicants.txt")
    graph.do_flow()
    print()

    print("Scenario 1")
    print("Applicants - > Employers")
    graph = Graph("../data/scenario1/Applicants.txt", "../data/scenario1/Employers.txt")
    graph.do_flow()
    print()

    print("Scenario 2")
    print("Employers -> Applicants")
    graph = Graph("../data/scenario2/Employers.txt", "../data/scenario2/Applicants.txt")
    graph.do_flow()
    print()

    print("Scenario 2")
    print("Applicants -> Employers")
    graph = Graph("../data/scenario2/Applicants.txt", "../data/scenario2/Employers.txt")
    graph.do_flow()


if __name__ == "__main__":
    main()