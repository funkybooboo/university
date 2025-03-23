from person import Person


class Applicant(Person):
    def __init__(self, name, priorities):
        """
        name is a string which uniquely identifies this person

        priorities is a list of strings which specifies a ranking of all
          potential partners, from best to worst
        """
        Person.__init__(self, name, priorities)
        # now compute a reverse lookup for efficient candidate rating
        self.ranking = {}
        for rank in range(len(priorities)):
            self.ranking[priorities[rank]] = rank


    def evaluate_proposal(self, suitor):
        """
        Evaluates a proposal, though does not enact it.

        suitor is the string identifier for the employer who is proposing

        returns True if proposal should be accepted, False otherwise
        """
        if suitor in self.ranking:
            if self.partner is None or self.ranking[suitor] < self.ranking[self.partner]:
                self.rank = self.ranking[suitor] + 1
                return True
        return False