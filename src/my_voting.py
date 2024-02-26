from numpy import random


def main():
    election = Election(20, 5, 1052)
    election.statistics()


class Election:
    class Voter:
        def __init__(self, name: str, pk: int, election):
            self.name = name
            self.pk = pk # primary key
            self.election = election
            self.connections = []
            self.candidates_ordered = []
            self.candidates_ranking = []
            self.__create_connections()
            self.__create_rankings()
            self.CANDIDATE = 0  # index of list which represents the candidate
            self.SCORE = 1  # index of list which represents the score of the candidate
            self.PLACE = 2  # index of list which represents the ranking, lowest is best

        def __create_connections(self):
            connection_count = round(random.uniform(0, self.election.voter_count / 2))
            for _ in range(connection_count):
                connect_to = random.randint(0, self.election.voter_count)
                if connect_to != self.pk:
                    self.connections[connect_to] = 1

        def __create_rankings(self):
            for candidate in range(self.election.candidate_count):
                self.candidates_ranking[candidate] = [candidate + 1, round(random.uniform(0, 100)) / 10, 0]

            s = sorted(self.candidates_ranking, reverse=True, key=lambda lst: lst[self.SCORE])
            self.candidates_ordered = [s[i][self.CANDIDATE] for i in range(self.election.candidate_count)]

            for v in range(self.election.candidate_count):
                candidate = s[v][self.CANDIDATE] - 1
                self.candidates_ranking[candidate][self.PLACE] = v + 1

        def print_connections(self):
            print(f"{self.name} {self.connections}")

        def print_rankings(self):
            print(f"{self.name} {self.candidates_ranking}")

    def __init__(self, voter_count: int, candidate_count: int, seed: int):
        random.seed(seed)
        self.voter_count = voter_count
        self.candidate_count = candidate_count
        self.voters = []
        for i in range(voter_count):
            self.voters.append(self.Voter(f"Voter{i}", i, self))

    def statistics(self):
        self.__print_connections()
        self.__print_rankings()

    def __print_connections(self):
        print("CONNECTIONS")
        for voter in self.voters:
            voter.print_connections()

    def __print_rankings(self):
        print("RANKINGS")
        for voter in self.voters:
            voter.print_rankings()


if __name__ == '__main__':
    main()
