class Person:
    """
    Represent a generic person
    """
    def __init__(self, name, priorities):
        """
        name is a string which uniquely identifies this person

        priorities is a list of strings which specifies a ranking of all
          potential partners, from best to worst
        """
        self.name = name
        self.priorities = priorities
        self.partner = None
        self.rank = None


    def __repr__(self):
        return 'Name is ' + self.name + '\n' + \
            'Partner is currently ' + str(self.partner) + str(self.rank) + '\n' + \
            'priority list is ' + str(self.priorities)