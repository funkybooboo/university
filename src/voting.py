from numpy import random


def main():
    election = Election(20, 5, 1052)
    election.statistics()


class Election:

    class Voter:
        def __init__(self, name: str, pk: int, election):
            self.name = name
            self.pk = pk  # primary key
            self.election = election
            self.connections = [0 for _ in range(self.election.voter_count)]
            self.candidates = []
            self.__create_connections()
            self.__rank_candidates()

        def __create_connections(self):
            connection_count = round(random.uniform(0, self.election.voter_count / 2))
            for _ in range(connection_count):
                connect_to = random.randint(0, self.election.voter_count)
                if connect_to != self.pk:
                    self.connections[connect_to] = 1

        def __rank_candidates(self):
            candidates = self.__create_candidates()
            self.candidates = sorted(candidates, key=lambda candidate: candidate["score"], reverse=True)
            self.__set_candidate_place()

        def __set_candidate_place(self):
            for candidate in self.candidates:
                candidate["place"] = self.candidates.index(candidate)

        def __create_candidates(self):
            candidates = []
            for i in range(self.election.candidate_count):
                candidate = {
                    "name": f"Candidate{i}",
                    "pk": i,
                    "score": round(random.randint(0, 100) / 10),
                    "place": 0
                }
                candidates.append(candidate)
            return candidates

        def print_connections(self):
            print(f"{self.name}  {self.connections}")

        def print_rankings(self):
            print(f"First choice for {self.name} is {self.candidates[0]['name']}")
            for candidate in self.candidates:
                print(f"\t{candidate['name']}: Score {candidate['score']}, Place {candidate['place']}")
            print(f"\tORDER: {[candidate['name'] for candidate in self.candidates]}")

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
