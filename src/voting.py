import numpy

CANDIDATE = 0  # index of list which represents the candidate
SCORE = 1  # index of list which represents the score of the candidate
PLACE = 2  # index of list which represents the ranking, lowest is best

def print_connections(names, c, voters):
    print("CONNECTIONS")
    for i in range(voters):
        print("%10s" % (names[i]), end=" ")
        for j in range(voters):
            print(c[i][j], end=' ')
        print()


def print_rankings(names, r, voters, candidates, ordered):
    print("CANDIDATE Rankings")
    for i in range(voters):
        print(f"First choice for {names[i]} is {ordered[i][CANDIDATE]}", end=" ")
        print(names[i], end=" ")
        for j in range(candidates):
            print(r[i][j], end='')
        print(" ORDER ", ordered[i])


def create_voting(voter_count: int, candidate_count: int):
    voter_names = []
    for i in range(voter_count):
        voter_names.append(f"Voter{i}")



    numpy.random.seed(1052)

    connections = [[0 for _ in range(voter_count)] for _ in range(voter_count)]
    ordered = [[] for _ in range(voter_count)]
    candidate_ranking = [[list() for _ in range(candidate_count)] for _ in range(voter_count)]

    for voter in range(voter_count):

        connection_count = round(numpy.random.uniform(0, voter_count / 2))
        for _ in range(connection_count):
            connect_to = numpy.random.randint(0, voter_count)
            if (connect_to != voter):
                connections[voter][connect_to] = 1

        for candidate in range(candidate_count):
            candidate_ranking[voter][candidate] = [candidate + 1, round(numpy.random.uniform(0, 100)) / 10, 0]

        s = sorted(candidate_ranking[voter], reverse=True, key=lambda v: v[SCORE])
        ordered[voter] = [s[i][CANDIDATE] for i in range(candidate_count)]

        for v in range(candidate_count):
            candidate = s[v][CANDIDATE] - 1
            candidate_ranking[voter][candidate][PLACE] = v + 1

    print_connections(voter_names, connections, voter_count)

    print_rankings(voter_names, candidate_ranking, voter_count, candidate_count, ordered)


if __name__ == '__main__':
    create_voting(20, 5)