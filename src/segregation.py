import matplotlib.pyplot as plt
import itertools
import random
import copy


class Segregation:
    def __init__(self, width, height, empty_ratio, similarity_threshold, n_iterations, colors=2):
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


    def populate(self):
        self.empty_houses = []
        self.agents = {}
        print("Populate ",  self.width ,  self.height)
        self.all_houses = list(itertools.product(range(self.width), range(self.height)))
        print(self.all_houses)
        random.shuffle(self.all_houses)

        self.n_empty = int(self.empty_ratio * len(self.all_houses))
        self.empty_houses = self.all_houses[:self.n_empty]

        self.remaining_houses = self.all_houses[self.n_empty:]
        houses_by_color = [self.remaining_houses[i::self.colors] for i in range(self.colors)]
        print("Houses by color ", houses_by_color[0])
        for i in range(self.colors):
            # create agents for each color
            dict2 = dict(zip(houses_by_color[i], [i + 1] * len(houses_by_color[i])))
            self.agents = {**self.agents, **dict2}
        print("dictionary",self.agents)


    def is_unsatisfied(self, x, y):

        myColor = self.agents[(x, y)]
        count_similar = 0
        count_different = 0

        count_different, count_similar = self.update_count(count_different, count_similar, myColor, x, y)

        if (count_similar + count_different) == 0:
            return False
        else:
            return float(count_similar) / (count_similar + count_different) < self.similarity_threshold


    def update_count(self, count_different, count_similar, my_color, x, y):
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


    def move_locations(self):
        total_distance=0
        for i in range(self.n_iterations):
            self.old_agents = copy.deepcopy(self.agents)
            n_changes = 0
            for agent in self.old_agents:
                if self.is_unsatisfied(agent[0], agent[1]):
                    agent_color = self.agents[agent]
                    empty_house = random.choice(self.empty_houses)
                    self.agents[empty_house] = agent_color
                    del self.agents[agent]
                    self.empty_houses.remove(empty_house)
                    self.empty_houses.append(agent)
                    total_distance += abs(empty_house[0] - agent[0])+ abs(empty_house[1] - agent[1])
                    n_changes += 1
            if i%30==0:
                print('Iteration: %d , Similarity Ratio: %3.2f. Number of changes: %d total distance: %d' %(i+1,self.similarity_threshold,n_changes,total_distance))
            if n_changes == 0:
                break


    def plot(self, title, file_name):
        fig, ax = plt.subplots()
        # If you want to run the simulation with more than 7 colors, you should set agent_colors accordingly
        agent_colors = {1: 'b', 2: 'r', 3: 'g', 4: 'c', 5: 'm', 6: 'y', 7: 'k'}
        marker_size = 150/self.width  # no logic here, I just played around with it
        for agent in self.agents:
            ax.scatter(agent[0] + 0.5, agent[1] + 0.5,s=marker_size, color=agent_colors[self.agents[agent]])

        ax.set_title(title, fontsize=10, fontweight='bold')
        ax.set_xlim([0, self.width])
        ax.set_ylim([0, self.height])
        ax.set_xticks([])
        ax.set_yticks([])
        plt.savefig(file_name)


    def calculate_similarity(self):
        similarity = []
        for agent in self.agents:
            count_similar = 0
            count_different = 0
            x = agent[0]
            y = agent[1]
            color = self.agents[(x, y)]
            count_different, count_similar = self.update_count(count_different, count_similar, color, x, y)
            try:
                similarity.append(float(count_similar) / (count_similar + count_different))
            except ZeroDivisionError:
                similarity.append(1)
        return sum(similarity) / len(similarity)

