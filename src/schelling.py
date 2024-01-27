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
    def __init__(self, width, height, empty_ratio, similarity_threshold, n_iterations, colors=2, run=1, verbose=False):
        self.agents = {}
        self.old_agents = {}
        self.width = width
        self.height = height
        self.colors = colors
        self.empty_ratio = empty_ratio
        self.similarity_threshold = similarity_threshold
        self.n_iterations = n_iterations
        self.all_houses = []
        self.n_empty = 0
        self.empty_houses = []
        self.remaining_houses = []
        self.verbose = verbose
        self.run = run
        self.__populate()


    def __populate(self):
        self.empty_houses = []
        self.agents = {}
        self.all_houses = list(product(range(self.width), range(self.height)))
        shuffle(self.all_houses)
        self.n_empty = int(self.empty_ratio * len(self.all_houses))
        self.empty_houses = self.all_houses[:self.n_empty]
        self.remaining_houses = self.all_houses[self.n_empty:]
        houses_by_color = [self.remaining_houses[i::self.colors] for i in range(self.colors)]
        for i in range(self.colors):
            # create agents for each color
            self.agents = {**self.agents, **dict(zip(houses_by_color[i], [i + 1] * len(houses_by_color[i])))}
        if self.verbose:
            print("Populate ", self.width, self.height)
            print(self.all_houses)
            print("Houses by color ", houses_by_color[0])
            print("dictionary",self.agents)


    def __is_unsatisfied(self, x, y):
        myColor = self.agents[(x, y)]
        count_similar = 0
        count_different = 0
        count_different, count_similar = self.__update_count(count_different, count_similar, myColor, x, y)
        try:
            return float(count_similar) / (count_similar + count_different) < self.similarity_threshold
        except ZeroDivisionError:
            return False


    def __update_count(self, count_different, count_similar, my_color, x, y):
        if x > 0 and y > 0 and (x - 1, y - 1) not in self.empty_houses:
            if self.agents[(x - 1, y - 1)] == my_color:
                count_similar += 1
            else:
                count_different += 1
        if y > 0 and (x, y - 1) not in self.empty_houses:
            if self.agents[(x, y - 1)] == my_color:
                count_similar += 1
            else:
                count_different += 1
        if x < (self.width - 1) and y > 0 and (x + 1, y - 1) not in self.empty_houses:
            if self.agents[(x + 1, y - 1)] == my_color:
                count_similar += 1
            else:
                count_different += 1
        if x > 0 and (x - 1, y) not in self.empty_houses:
            if self.agents[(x - 1, y)] == my_color:
                count_similar += 1
            else:
                count_different += 1
        if x < (self.width - 1) and (x + 1, y) not in self.empty_houses:
            if self.agents[(x + 1, y)] == my_color:
                count_similar += 1
            else:
                count_different += 1
        if x > 0 and y < (self.height - 1) and (x - 1, y + 1) not in self.empty_houses:
            if self.agents[(x - 1, y + 1)] == my_color:
                count_similar += 1
            else:
                count_different += 1
        if x > 0 and y < (self.height - 1) and (x, y + 1) not in self.empty_houses:
            if self.agents[(x, y + 1)] == my_color:
                count_similar += 1
            else:
                count_different += 1
        if x < (self.width - 1) and y < (self.height - 1) and (x + 1, y + 1) not in self.empty_houses:
            if self.agents[(x + 1, y + 1)] == my_color:
                count_similar += 1
            else:
                count_different += 1
        return count_different, count_similar


    def __calculate_similarity(self):
        similarity = []
        for agent in self.agents:
            count_similar = 0
            count_different = 0
            x = agent[0]
            y = agent[1]
            color = self.agents[(x, y)]
            count_different, count_similar = self.__update_count(count_different, count_similar, color, x, y)
            try:
                similarity.append(float(count_similar) / (count_similar + count_different))
            except ZeroDivisionError:
                similarity.append(1)
        return sum(similarity) / len(similarity)


    def __move_locations(self):
        total_distance=0
        for i in range(self.n_iterations):
            self.old_agents = deepcopy(self.agents)
            n_changes = 0
            for agent in self.old_agents:
                if self.__is_unsatisfied(agent[0], agent[1]):
                    agent_color = self.agents[agent]
                    empty_house = choice(self.empty_houses)
                    self.agents[empty_house] = agent_color
                    del self.agents[agent]
                    self.empty_houses.remove(empty_house)
                    self.empty_houses.append(agent)
                    total_distance += abs(empty_house[0] - agent[0]) + abs(empty_house[1] - agent[1])
                    n_changes += 1
            if i % 30 == 0 and self.verbose:
                print('Iteration: %d , Similarity Ratio: %3.2f. Number of changes: %d total distance: %d' %(i + 1, self.similarity_threshold, n_changes,total_distance))
            if n_changes == 0:
                break


    def __plot(self, title, file_name):
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
        plt.savefig(f"../data/run{self.run}/{file_name}")


    def simulate(self):
        num_colors = self.colors
        happiness_threshold = int(self.similarity_threshold * 100)
        self.__plot(f'Schelling Model with {num_colors} colors: Initial State', f'schelling_{happiness_threshold}_initial.png')
        self.__move_locations()
        self.__plot(f'Schelling Model with {num_colors} colors: Final State with Happiness Threshold {happiness_threshold}%', f'schelling_{happiness_threshold}_final.png')

