# Author: Nate Stott
# Date: 2/26/2024
# For: CS5110 - Multi-Agent Systems - Program 4

from copy import deepcopy
from numpy import random
from random import randint


def main():
    seed = 1052
    print("SIMULATION 1")
    Election.simulation(10, 5, seed, True)
    print("SIMULATION 2")
    Election.simulation(20, 5, seed, True)
    print("SIMULATION 3")
    Election.simulation(30, 5, seed)
    print("SIMULATION 4")
    Election.simulation(40, 5, seed)
    print("SIMULATION 5")
    Election.simulation(50, 5, seed)
    print("RANDOM SIMULATION")
    Election.simulation(randint(2, 1000), randint(2, 10), seed)


class Election:
    class Voter:
        def __init__(self, name: str, pk: int, election):
            self.name = name
            self.pk = pk  # primary key
            self.election = election
            self.connections = [0 for _ in range(self.election.voter_count)]
            self.ranked_candidates = []
            self.__create_connections()
            self.__create_candidates()
            self.__rank_candidates()
            self.original_ranked_candidates = deepcopy(self.ranked_candidates)

        def __rank_candidates(self):
            self.ranked_candidates = sorted(self.ranked_candidates, key=lambda candidate: candidate["score"], reverse=True)
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
            self.ranked_candidates = candidates

        def print_connections(self):
            connections = []
            for pk, connection in enumerate(self.connections):
                if connection == 1:
                    connections.append(self.election.voters[pk].name)
            print(f"{self.name}: {connections}")

        def print_rankings(self):
            print(f"First choice for {self.name} is {self.ranked_candidates[0]['name']}")
            for candidate in self.ranked_candidates:
                print(f"\t{candidate['name']}: Score {candidate['score']}, Place {candidate['place']}")
            print(f"\tORDER: {[candidate['name'] for candidate in self.ranked_candidates]}")

        def cardinal_utility(self, winner_pk: int):
            for candidate in self.ranked_candidates:
                if candidate["pk"] == winner_pk:
                    return abs(self.ranked_candidates[0]["score"] - candidate["score"])
            return None

        def ordinal_utility(self, winner_pk: int):
            for candidate in self.ranked_candidates:
                if candidate["pk"] == winner_pk:
                    return abs(self.ranked_candidates[0]["place"] - candidate["place"])
            return None

        def vote(self):
            return self.ranked_candidates[0]["pk"]

        def social_network_vote(self):
            connections_vote_information, connection_count = self.__get_connections_vote_information()
            if connection_count == 0:
                # I have no friends
                return self.vote()
            connections_candidates_information = self.__get_candidates_information(connections_vote_information)
            sorted_connections_candidates_information = sorted(connections_candidates_information,
                                                               key=lambda candidate: candidate["vote_count"],
                                                               reverse=True)
            return self.__get_social_vote(sorted_connections_candidates_information, connection_count)

        def __get_social_vote(self, sorted_connections_candidates_information, connection_count):
            vote_pk = self.vote()
            # if the candidate I would like to for is the most popular among my friends, I will vote for them
            if sorted_connections_candidates_information[0]["pk"] == vote_pk:
                vote_pk = sorted_connections_candidates_information[0]["pk"]
            else:
                vote_pk = self.__get_candidate_im_ok_with_and_is_likely_to_win(connection_count,
                                                                               sorted_connections_candidates_information,
                                                                               vote_pk)
            return vote_pk

        def __get_candidate_im_ok_with_and_is_likely_to_win(self, connection_count,
                                                            sorted_connections_candidates_information, vote_pk):
            my_average_score = self.__get_my_average_score()
            for candidate in sorted_connections_candidates_information:
                is_likely_to_win = candidate["vote_count"] >= connection_count / 2
                if is_likely_to_win:
                    my_score, my_place = self.__get_how_i_feel_about_candidate(candidate["pk"])
                    if my_score is None or my_place is None:
                        # I don't know about this candidate
                        continue

                    # Talking to a couple of people about how they would define "ok with winning" and I got a couple different answers:
                    # - "I would be ok with them winning if they are in the top 3"
                    # - "I would be ok with them winning if they are in the top 50%" and "I would be ok with them winning if they are in the top 25%"
                    # it seems like the consensus is that they would be ok with them winning if they are in the top half or top quarter

                    # I defined "likely to win" as my score for this candidate being greater than or equal to my average score,
                    # I think this is a good way to define it because of some examples I thought of
                    # - If there are 3 candidates and I gave the following scores: first got 10, second got 9.9, and the third got 9.8 then my average score is 9.9.
                    # That means I am ok with the first or second winning.
                    # I think this makes sense because I generally liked everyone running, so anyone winning is ok with me.
                    # - If there are 5 candidates and I gave the following scores: first got 10, second got 1, and the rest got 0 then my average score is 2.2.
                    # That means I am ok with only the first winning, and I am ok with a candidate winning if they have at least a score of 2.2.
                    # I think the reason why such a low score makes sense is that because I generally hated everyone running, they all brought my standards down.

                    # I don't care about the placement of the candidates, I only care about the scores because of those examples
                    # Better than average makes sense to me because if they are better than average, then they have to be alright.

                    i_am_ok_with_them_winning = my_score >= my_average_score
                    if i_am_ok_with_them_winning:
                        vote_pk = candidate["pk"]
                        break
                else:
                    # They are not likely to win, and because the candidates are sorted by vote count, no one else is likely to win either
                    break
            return vote_pk

        def __get_my_average_score(self):
            my_average_score = 0
            for candidate in self.ranked_candidates:
                my_average_score += candidate["score"]
            return my_average_score / len(self.ranked_candidates)

        def __get_how_i_feel_about_candidate(self, candidate_pk):
            for candidate in self.ranked_candidates:
                if candidate["pk"] == candidate_pk:
                    return candidate["score"], candidate["place"]
            return None, None

        def __get_candidates_information(self, connections_vote_information):
            candidates_information = []
            for candidate in self.ranked_candidates:
                candidates_information.append({
                    "pk": candidate["pk"],
                    "name": candidate["name"],
                    "average_score": self.__get_average_score(candidate["pk"], connections_vote_information),
                    "average_place": self.__get_average_place(candidate["pk"], connections_vote_information),
                    "vote_count": self.__get_candidate_vote_count(candidate["pk"], connections_vote_information)
                })
            return candidates_information

        @staticmethod
        def __get_average_place(candidate_pk: int, connections_vote_information):
            place_sum = 0
            vote_count = 0
            for vote in connections_vote_information:
                if vote["vote_pk"] == candidate_pk:
                    place_sum += vote["vote_place"]
                    vote_count += 1
            if vote_count == 0:
                return 0
            return place_sum / vote_count

        @staticmethod
        def __get_average_score(candidate_pk: int, connections_vote_information):
            score_sum = 0
            vote_count = 0
            for vote in connections_vote_information:
                if vote["vote_pk"] == candidate_pk:
                    score_sum += vote["vote_score"]
                    vote_count += 1
            if vote_count == 0:
                return 0
            return score_sum / vote_count

        @staticmethod
        def __get_candidate_vote_count(candidate_pk: int, connections_vote_information):
            vote_count = 0
            for vote in connections_vote_information:
                if vote["vote_pk"] == candidate_pk:
                    vote_count += 1
            if vote_count == 0:
                return 0
            return vote_count

        def __get_connections_vote_information(self):
            connection_votes = []
            connection_count = 0
            for pk, connection in enumerate(self.connections):
                if connection == 1:
                    connection_count += 1
                    connection_votes.append({
                        "pk": pk,
                        "name": self.election.voters[pk].name,
                        "vote_name": self.election.voters[pk].ranked_candidates[0]["name"],
                        "vote_pk": self.election.voters[pk].ranked_candidates[0]["pk"],
                        "vote_score": self.election.voters[pk].ranked_candidates[0]["score"],
                        "vote_place": self.election.voters[pk].ranked_candidates[0]["place"]
                    })
            return connection_votes, connection_count

        def remove_candidate(self, candidate_pk: int):
            new_ranked_candidates = []
            for candidate in self.ranked_candidates:
                if candidate["pk"] != candidate_pk:
                    new_ranked_candidates.append(candidate)
            self.ranked_candidates = new_ranked_candidates
            self.__rank_candidates()

        def rest_candidates(self):
            self.ranked_candidates = deepcopy(self.original_ranked_candidates)

    def __init__(self, voter_count: int, candidate_count: int, seed: int, verbose: bool = False):
        random.seed(seed)
        self.voter_count = voter_count
        self.candidate_count = candidate_count
        self.original_candidate_count = candidate_count
        self.verbose = verbose
        self.voters = []
        for i in range(voter_count):
            self.voters.append(self.Voter(f"Voter{i}", i, self))

    def statistics(self):
        print("STATISTICS")
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
        self.candidate_count -= 1

    def __voter_welfare(self, winner_pk: int, is_printing: bool = True):
        average_cu = 0
        average_ou = 0
        for voter in self.voters:
            cu = voter.cardinal_utility(winner_pk)
            ou = voter.ordinal_utility(winner_pk)
            if cu is None or ou is None:
                raise ValueError("Could not find winner in voter's rankings.")
            if is_printing and self.verbose:
                print(f"{voter.name} Cardinal Utility: {cu} Ordinal Utility: {ou}")
            average_cu += cu
            average_ou += ou
        average_cu /= len(self.voters)
        average_ou /= len(self.voters)
        return average_cu, average_ou

    def __reset_candidates(self):
        self.candidate_count = self.original_candidate_count
        for voter in self.voters:
            voter.rest_candidates()

    def first_past_the_post_voting(self, is_social_network: bool):
        self.__reset_candidates()
        if is_social_network:
            winner_pk, loser_pk, minds_changed = self.__social_network_vote()
            if self.verbose:
                print(f"MINDS CHANGED: {minds_changed}")
        else:
            winner_pk, loser_pk = self.__vote()
        if self.verbose:
            print(f"WINNER: Candidate{winner_pk}")
            average_cu, average_ou = self.__voter_welfare(winner_pk)
            print("AVERAGE CARDINAL UTILITY:", average_cu)
            print("AVERAGE ORDINAL UTILITY:", average_ou)
        return winner_pk

    def borda_voting(self):
        self.__reset_candidates()
        points = [i + 1 for i in range(self.candidate_count)]
        points = list(reversed(points))
        candidate_points = [0 for _ in range(self.candidate_count)]
        for voter in self.voters:
            for candidate in voter.ranked_candidates:
                candidate_points[candidate["pk"]] += points[candidate["place"]]
        winner_pk = candidate_points.index(max(candidate_points))
        if self.verbose:
            print(f"WINNER: Candidate{winner_pk}")
            average_cu, average_ou = self.__voter_welfare(winner_pk)
            print("AVERAGE CARDINAL UTILITY:", average_cu)
            print("AVERAGE ORDINAL UTILITY:", average_ou)
        return winner_pk

    def ranked_choice_voting(self, is_social_network: bool):
        self.__reset_candidates()
        for _ in range(self.candidate_count - 1):
            if is_social_network:
                winner_pk, loser_pk, minds_changed = self.__social_network_vote()
                if self.verbose:
                    print(f"MINDS CHANGED: {minds_changed}")
            else:
                winner_pk, loser_pk = self.__vote()
            if self.verbose:
                print(f"ROUND WINNER: Candidate{winner_pk}")
                print(f"ROUND LOSER: Candidate{loser_pk}")
                average_cu, average_ou = self.__voter_welfare(winner_pk)
                print("AVERAGE CARDINAL UTILITY:", average_cu)
                print("AVERAGE ORDINAL UTILITY:", average_ou)
                print()
            self.__remove_candidate(loser_pk)
        if is_social_network:
            winner_pk, loser_pk, minds_changed = self.__social_network_vote()
            if self.verbose:
                print(f"MINDS CHANGED: {minds_changed}")
        else:
            winner_pk, loser_pk = self.__vote()
        if self.verbose:
            print(f"WINNER: Candidate{winner_pk}")
        self.__voter_welfare(winner_pk)
        return winner_pk

    def __vote(self):
        votes = dict()
        for voter in self.voters:
            vote_pk = voter.vote()
            if vote_pk not in votes:
                votes[vote_pk] = 1
            else:
                votes[vote_pk] += 1
        winner_pk = max(votes, key=votes.get)
        loser_pk = min(votes, key=votes.get)
        return winner_pk, loser_pk

    def __social_network_vote(self):
        votes = dict()
        minds_changed = 0
        for voter in self.voters:
            nv = voter.social_network_vote()
            v = voter.vote()
            if nv not in votes:
                votes[nv] = 1
            else:
                votes[nv] += 1
            if nv != v:
                minds_changed += 1
        winner_pk = max(votes, key=votes.get)
        loser_pk = min(votes, key=votes.get)
        return winner_pk, loser_pk, minds_changed

    @staticmethod
    def simulation(voter_count: int, candidate_count: int, seed: int, verbose: bool = False):
        print("*" * 50)
        print()
        print(f"VOTER COUNT: {voter_count}")
        print(f"CANDIDATE COUNT: {candidate_count}")
        print(f"SEED: {seed}")
        print(f"VERBOSE: {verbose}")
        if verbose:
            print()
        election = Election(voter_count, candidate_count, seed, verbose)
        if verbose:
            print()
            election.statistics()
            print()
            print("FIRST PAST THE POST VOTING")
        winner_pk_fptp = election.first_past_the_post_voting(False)
        if verbose:
            print()
            print("RANKED CHOICE VOTING")
        winner_pk_rc = election.ranked_choice_voting(False)
        if verbose:
            print()
            print("FIRST PAST THE POST VOTING SOCIAL NETWORK")
        winner_pk_fptpn = election.first_past_the_post_voting(True)
        if verbose:
            print()
            print("RANKED CHOICE VOTING SOCIAL NETWORK")
        winner_pk_rcn = election.ranked_choice_voting(True)
        if verbose:
            print()
            print("BORDA VOTING")
        winner_pk_b = election.borda_voting()
        print()
        print("--Comparing Voting Methods--")
        average_cu_fptp, average_ou_fptp = election.__voter_welfare(winner_pk_fptp, False)
        average_cu_rc, average_ou_rc = election.__voter_welfare(winner_pk_rc, False,)
        average_cu_fptpn, average_ou_fptpn = election.__voter_welfare(winner_pk_fptpn, False)
        average_cu_rcn, average_ou_rcn = election.__voter_welfare(winner_pk_rcn, False)
        average_cu_b, average_ou_b = election.__voter_welfare(winner_pk_b, False)
        print(f"FIRST PAST THE POST VOTING WINNER: Candidate{winner_pk_fptp}")
        print(f"\tAVERAGE CARDINAL UTILITY: {average_cu_fptp}")
        print(f"\tAVERAGE ORDINAL UTILITY: {average_ou_fptp}")
        print(f"RANKED CHOICE VOTING WINNER: Candidate{winner_pk_rc}")
        print(f"\tAVERAGE CARDINAL UTILITY: {average_cu_rc}")
        print(f"\tAVERAGE ORDINAL UTILITY: {average_ou_rc}")
        print(f"FIRST PAST THE POST VOTING SOCIAL NETWORK WINNER: Candidate{winner_pk_fptpn}")
        print(f"\tAVERAGE CARDINAL UTILITY: {average_cu_fptpn}")
        print(f"\tAVERAGE ORDINAL UTILITY: {average_ou_fptpn}")
        print(f"RANKED CHOICE VOTING SOCIAL NETWORK WINNER: Candidate{winner_pk_rcn}")
        print(f"\tAVERAGE CARDINAL UTILITY: {average_cu_rcn}")
        print(f"\tAVERAGE ORDINAL UTILITY: {average_ou_rcn}")
        print(f"BORDA VOTING WINNER: Candidate{winner_pk_b}")
        print(f"\tAVERAGE CARDINAL UTILITY: {average_cu_b}")
        print(f"\tAVERAGE ORDINAL UTILITY: {average_ou_b}")
        print()
        print("*" * 50)


if __name__ == '__main__':
    main()
