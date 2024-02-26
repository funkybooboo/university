from copy import deepcopy
from numpy import random


def main():
    Election.simulation(20, 5, 1052, True)
    Election.simulation(100, 5, 1052)
    Election.simulation(1000, 10, 1052)


class Election:
    class Voter:
        def __init__(self, name: str, pk: int, election):
            self.name = name
            self.pk = pk  # primary key
            self.election = election
            self.connections = [0 for _ in range(self.election.voter_count)]
            self.ranked_candidates = []
            self.__create_connections()
            self.__rank_candidates()
            self.original_ranked_candidates = deepcopy(self.ranked_candidates)

        def __create_connections(self):
            connection_count = round(random.uniform(0, self.election.voter_count / 2))
            for _ in range(connection_count):
                connect_to = random.randint(0, self.election.voter_count)
                if connect_to != self.pk:
                    self.connections[connect_to] = 1

        def __rank_candidates(self):
            candidates = self.__create_candidates()
            self.ranked_candidates = sorted(candidates, key=lambda candidate: candidate["score"], reverse=True)
            self.__set_candidate_place()

        def __set_candidate_place(self):
            for candidate in self.ranked_candidates:
                candidate["place"] = self.ranked_candidates.index(candidate)

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
            print(f"First choice for {self.name} is {self.ranked_candidates[0]['name']}")
            for candidate in self.ranked_candidates:
                print(f"\t{candidate['name']}: Score {candidate['score']}, Place {candidate['place']}")
            print(f"\tORDER: {[candidate['name'] for candidate in self.ranked_candidates]}")

        def cardinal_utility(self, winner_pk: int):
            return abs(self.ranked_candidates[0]["score"] - self.ranked_candidates[winner_pk]["score"])

        def ordinal_utility(self, winner_pk: int):
            return abs(self.ranked_candidates[0]["place"] - self.ranked_candidates[winner_pk]["place"])

        def vote(self):
            return self.ranked_candidates[0]["pk"]

        def remove_candidate(self, candidate_pk: int):
            for candidate in self.ranked_candidates:
                if candidate["pk"] == candidate_pk:
                    self.ranked_candidates.remove(candidate)
                    break

    def __init__(self, voter_count: int, candidate_count: int, seed: int, verbose: bool = False):
        random.seed(seed)
        self.voter_count = voter_count
        self.candidate_count = candidate_count
        self.verbose = verbose
        self.voters = []
        for i in range(voter_count):
            self.voters.append(self.Voter(f"Voter{i}", i, self))

    def statistics(self):
        if self.verbose:
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

    def __remove_candidate(self, candidate_pk: int):
        for voter in self.voters:
            voter.remove_candidate(candidate_pk)

    def __vote(self):
        votes = [0 for _ in range(self.candidate_count)]
        for voter in self.voters:
            votes[voter.vote()] += 1
        winner_pk = votes.index(max(votes))
        loser_pk = votes.index(min(votes))
        return winner_pk, loser_pk

    def __voter_welfare(self, winner_pk: int):
        for voter in self.voters:
            print(f"{voter.name}")
            print(f"\tCardinal Utility: {voter.cardinal_utility(winner_pk)}")
            print(f"\tOrdinal Utility: {voter.ordinal_utility(winner_pk)}")

    def __reset_rankings(self):
        for voter in self.voters:
            voter.ranked_candidates = deepcopy(voter.original_ranked_candidates)

    def first_past_the_post_voting(self, is_social_network: bool = False):
        print("FIRST PAST THE POST")
        self.__reset_rankings()
        if is_social_network:
            self.social_network()
        winner_pk, loser_pk = self.__vote()
        print("WINNER:", winner_pk)
        self.__voter_welfare(winner_pk)

    def ranked_choice_voting(self, is_social_network: bool = False):
        print("RANKED CHOICE")
        self.__reset_rankings()
        for _ in range(self.candidate_count - 1):
            if is_social_network:
                self.social_network()
            winner_pk, loser_pk = self.__vote()
            if self.verbose:
                print(f"ROUND WINNER: Candidate{winner_pk}")
                print(f"ROUND LOSER: Candidate{loser_pk}")
                self.__voter_welfare(winner_pk)
            self.__remove_candidate(loser_pk)
        winner_pk, loser_pk = self.__vote()
        print(f"WINNER: Candidate{winner_pk}")
        self.__voter_welfare(winner_pk)

    def social_network(self):
        pass

    @staticmethod
    def simulation(voter_count: int, candidate_count: int, seed: int, verbose: bool = False):
        print("*" * 50)
        election = Election(voter_count, candidate_count, seed, verbose)
        print()
        election.statistics()
        print()
        election.first_past_the_post_voting()
        print()
        election.ranked_choice_voting()
        print()
        election.first_past_the_post_voting(True)
        print()
        election.ranked_choice_voting(True)


if __name__ == '__main__':
    main()
