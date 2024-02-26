from copy import deepcopy
from numpy import random


def main():
    Election.simulation(20, 5, 1052, True)
    Election.simulation(100, 5, 1052)
    Election.simulation(1_000, 5, 1052)
    Election.simulation(10_000, 5, 1052)
    Election.simulation(100_000, 5, 1052)
    Election.simulation(1_000_000, 5, 1052)


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

        def __rank_candidates(self):
            if len(self.ranked_candidates) == 0:
                candidates = self.__create_candidates()
            else:
                candidates = self.ranked_candidates
            self.ranked_candidates = sorted(candidates, key=lambda candidate: candidate["score"], reverse=True)
            self.__set_candidate_place()

        def __create_connections(self):
            connection_count = round(random.uniform(0, self.election.voter_count / 2))
            for _ in range(connection_count):
                connect_to = random.randint(0, self.election.voter_count)
                if connect_to != self.pk:
                    self.connections[connect_to] = 1

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

        def social_network_vote(self):
            connections_vote_information = self.__get_connections_vote_information()
            if len(connections_vote_information) == 0:
                return self.ranked_candidates[0]["pk"]
            return self.__get_my_vote(connections_vote_information)

        def __get_my_vote(self, connections_vote_information):
            my_vote = self.ranked_candidates[0]["pk"]
            for connection_vote_information in connections_vote_information:
                # TODO look through connections and make a decision about who is the best candidate for me and who is likely to win based on my connections
                pass
            return my_vote

        def __get_connections_vote_information(self):
            connection_votes = []
            for pk, connection in enumerate(self.connections):
                if connection == 1:
                    connection_votes.append({
                        "pk": pk,
                        "name": self.election.voters[pk].name,
                        "vote_name": self.election.voters[pk].ranked_candidates[0]["name"],
                        "vote_pk": self.election.voters[pk].ranked_candidates[0]["pk"],
                        "vote_score": self.election.voters[pk].ranked_candidates[0]["score"],
                        "vote_place": self.election.voters[pk].ranked_candidates[0]["place"]
                    })
            return connection_votes

        def remove_candidate(self, candidate_pk: int):
            for candidate in self.ranked_candidates:
                if candidate["pk"] == candidate_pk:
                    self.ranked_candidates.remove(candidate)
                    break
            self.__rank_candidates()

        def rest_ranked_candidates(self):
            self.ranked_candidates = deepcopy(self.original_ranked_candidates)

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

    def __voter_welfare(self, winner_pk: int):
        for voter in self.voters:
            print(f"{voter.name}")
            print(f"\tCardinal Utility: {voter.cardinal_utility(winner_pk)}")
            print(f"\tOrdinal Utility: {voter.ordinal_utility(winner_pk)}")

    def __reset_candidates(self):
        for voter in self.voters:
            voter.rest_ranked_candidates()

    def first_past_the_post_voting(self, is_social_network: bool):
        print("FIRST PAST THE POST")
        self.__reset_candidates()
        if is_social_network:
            winner_pk, loser_pk = self.__social_network_vote()
        else:
            winner_pk, loser_pk = self.__vote()
        print("WINNER:", winner_pk)
        self.__voter_welfare(winner_pk)

    def ranked_choice_voting(self, is_social_network: bool):
        print("RANKED CHOICE")
        self.__reset_candidates()
        for _ in range(self.candidate_count - 1):
            if is_social_network:
                winner_pk, loser_pk = self.__social_network_vote()
            else:
                winner_pk, loser_pk = self.__vote()
            if self.verbose:
                print(f"ROUND WINNER: Candidate{winner_pk}")
                print(f"ROUND LOSER: Candidate{loser_pk}")
                self.__voter_welfare(winner_pk)
            self.__remove_candidate(loser_pk)
        if is_social_network:
            winner_pk, loser_pk = self.__social_network_vote()
        else:
            winner_pk, loser_pk = self.__vote()
        print(f"WINNER: Candidate{winner_pk}")
        self.__voter_welfare(winner_pk)

    def __vote(self):
        votes = [0 for _ in range(self.candidate_count)]
        for voter in self.voters:
            votes[voter.vote()] += 1
        winner_pk = votes.index(max(votes))
        loser_pk = votes.index(min(votes))
        return winner_pk, loser_pk

    def __social_network_vote(self):
        votes = [0 for _ in range(self.candidate_count)]
        for voter in self.voters:
            votes[voter.social_network_vote()] += 1
        winner_pk = votes.index(max(votes))
        loser_pk = votes.index(min(votes))
        return winner_pk, loser_pk

    @staticmethod
    def simulation(voter_count: int, candidate_count: int, seed: int, verbose: bool = False):
        print("*" * 50)
        print(f"VOTER COUNT: {voter_count}")
        print(f"CANDIDATE COUNT: {candidate_count}")
        print(f"SEED: {seed}")
        print(f"VERBOSE: {verbose}")
        print()
        election = Election(voter_count, candidate_count, seed, verbose)
        print()
        election.statistics()
        print()
        election.first_past_the_post_voting(False)
        print()
        election.ranked_choice_voting(False)
        print()
        election.first_past_the_post_voting(True)
        print()
        election.ranked_choice_voting(True)
        print()


if __name__ == '__main__':
    main()
