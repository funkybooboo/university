import matplotlib.pyplot as plt
from itertools import product
from random import shuffle, choice
from copy import deepcopy


# todo a. Add output to indicate the percentage of each agent type that meets the desired similarity_threshold.

# todo b. Add a feature where the similarity_threshold can be different for each of the colors.

# todo c. Add a feature to stop when little progress is being made. Print out a message to indicate how many iterations you did.

# todo d. Add a feature where agents can improve on their current location by performing a location swap with another agent who is willing to swap (in addition to just swapping with an open position).

# todo e. Add a feature where you prefer to move to locations in the neighborhood. The idea is that a move may be cheaper if the agent didn't move as far. Define neighborhood however you like.

# todo f. Explore something else you consider interesting.


class Schelling:
    """
    This class implements the Schelling model of segregation.

    Note:
    - self.agents is a dictionary of the form {(x,y): color, ...}
        - color is an integer between 1 and self.colors
        - (x, y) is a tuple representing the location of the agent
    - self.empty_spaces is a list of tuples (x,y) representing empty spaces
    - self.width and self.height are the dimensions of the grid
    - self.similarity_threshold is a float between 0 and 1
    - self.verbose is a boolean that indicates whether to print or not
    """

    def __init__(self, width, height, empty_ratio, similarity_threshold, colors=2, verbose=False):
        self.agents = {}
        self.width = width
        self.height = height
        self.colors = colors
        self.similarity_threshold = similarity_threshold
        self.verbose = verbose
        self.__populate(empty_ratio)


    def __populate(self, empty_ratio):
        self.empty_spaces = []
        self.agents = {}
        all_spaces = list(product(range(self.width), range(self.height)))
        shuffle(all_spaces)
        num_empty = int(empty_ratio * len(all_spaces))
        self.empty_spaces = all_spaces[:num_empty]
        remaining_spaces = all_spaces[num_empty:]
        houses_by_color = [remaining_spaces[i::self.colors] for i in range(self.colors)]
        for i in range(self.colors):
            # create agents for each color
            self.agents = {**self.agents, **dict(zip(houses_by_color[i], [i + 1] * len(houses_by_color[i])))}
        if self.verbose:
            print("Populate ", self.width, self.height)
            print(all_spaces)
            print("Houses by color ", houses_by_color[0])
            print("dictionary",self.agents)


    def __is_unsatisfied(self, agent):
        num_different, num_similar = self.__get_similar_and_different_count(agent)
        try:
            return float(num_similar) / (num_similar + num_different) < self.similarity_threshold
        except ZeroDivisionError:
            return False


    def __get_similar_and_different_count(self, agent):
        num_similar = 0
        num_different = 0
        color = self.agents[agent]
        x = agent[0]
        y = agent[1]
        if x > 0 and y > 0 and (x - 1, y - 1) not in self.empty_spaces:
            if self.agents[(x - 1, y - 1)] == color:
                num_similar += 1
            else:
                num_different += 1
        if y > 0 and (x, y - 1) not in self.empty_spaces:
            if self.agents[(x, y - 1)] == color:
                num_similar += 1
            else:
                num_different += 1
        if x < (self.width - 1) and y > 0 and (x + 1, y - 1) not in self.empty_spaces:
            if self.agents[(x + 1, y - 1)] == color:
                num_similar += 1
            else:
                num_different += 1
        if x > 0 and (x - 1, y) not in self.empty_spaces:
            if self.agents[(x - 1, y)] == color:
                num_similar += 1
            else:
                num_different += 1
        if x < (self.width - 1) and (x + 1, y) not in self.empty_spaces:
            if self.agents[(x + 1, y)] == color:
                num_similar += 1
            else:
                num_different += 1
        if x > 0 and y < (self.height - 1) and (x - 1, y + 1) not in self.empty_spaces:
            if self.agents[(x - 1, y + 1)] == color:
                num_similar += 1
            else:
                num_different += 1
        if x > 0 and y < (self.height - 1) and (x, y + 1) not in self.empty_spaces:
            if self.agents[(x, y + 1)] == color:
                num_similar += 1
            else:
                num_different += 1
        if x < (self.width - 1) and y < (self.height - 1) and (x + 1, y + 1) not in self.empty_spaces:
            if self.agents[(x + 1, y + 1)] == color:
                num_similar += 1
            else:
                num_different += 1
        return num_different, num_similar


    def __calculate_space_similarity(self):
        similarity = []
        for agent in self.agents:
            count_different, count_similar = self.__get_similar_and_different_count(agent)
            try:
                similarity.append(float(count_similar) / (count_similar + count_different))
            except ZeroDivisionError:
                similarity.append(1)
        return sum(similarity) / len(similarity)


    def __move_agents_locations(self, num_iterations):
        total_distance=0
        for i in range(num_iterations):
            old_agents = deepcopy(self.agents)
            num_changes = 0
            for agent in old_agents:
                if self.__is_unsatisfied(agent):
                    agent_color = self.agents[agent]
                    empty_house = choice(self.empty_spaces)
                    self.agents[empty_house] = agent_color
                    del self.agents[agent]
                    self.empty_spaces.remove(empty_house)
                    self.empty_spaces.append(agent)
                    total_distance += abs(empty_house[0] - agent[0]) + abs(empty_house[1] - agent[1])
                    num_changes += 1
            if i % 30 == 0 and self.verbose:
                print('Iteration: %d , Similarity Ratio: %3.2f. Number of changes: %d total distance: %d' %(i + 1, self.similarity_threshold, num_changes,total_distance))
            if num_changes == 0:
                break


    def __plot(self, title, file_name, run):
        fig, ax = plt.subplots()
        # If you want to run the simulation with more than 7 colors, you should set agent_colors accordingly
        agent_colors = {1: 'b', 2: 'r', 3: 'g', 4: 'c', 5: 'm', 6: 'y', 7: 'k'}
        marker_size = 150 / self.width  # no logic here, I just played around with it
        for agent in self.agents:
            ax.scatter(agent[0] + 0.5, agent[1] + 0.5, s=marker_size, color=agent_colors[self.agents[agent]])
        ax.set_title(title, fontsize=10, fontweight='bold')
        ax.set_xlim([0, self.width])
        ax.set_ylim([0, self.height])
        ax.set_xticks([])
        ax.set_yticks([])
        plt.savefig(f"../data/run{run}/{file_name}")


    def simulate(self, run, num_iterations):
        num_colors = self.colors
        happiness_threshold = int(self.similarity_threshold * 100)
        self.__plot(f'Schelling Model with {num_colors} colors: Initial State', f'schelling_{happiness_threshold}_initial.png', run)
        self.__move_agents_locations(num_iterations)
        self.__plot(f'Schelling Model with {num_colors} colors: Final State with Happiness Threshold {happiness_threshold}%', f'schelling_{happiness_threshold}_final.png', run)

