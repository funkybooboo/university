from person import Person


class Employer(Person):
    def __init__(self, name, priorities):
        """
        name is a string which uniquely identifies this person

        priorities is a list of strings which specifies a ranking of all
          potential partners, from best to worst
        """
        Person.__init__(self, name, priorities)
        self.proposal_index = 0  # next person in our list to whom we might propose


    def next_proposal(self):
        if self.proposal_index >= len(self.priorities):
            print('returned None')
            return None
        goal = self.priorities[self.proposal_index]
        self.proposal_index += 1
        return goal


    def __repr__(self):
        return Person.__repr__(self) + '\n' + \
            'next proposal would be to person at position ' + str(self.proposal_index)