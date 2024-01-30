import matplotlib.pyplot as plt
from itertools import product
from random import shuffle, choice
from copy import deepcopy


# a. Add output to indicate the percentage of each agent type that meets the desired similarity_threshold.

# b. Add a feature where the similarity_threshold can be different for each of the colors.

# c. Add a feature to stop when little progress is being made. Print out a message to indicate how many iterations you did.

# d. Add a feature where agents can improve on their current location by performing a location swap with another agent who is willing to swap (in addition to just swapping with an open position).

# e. Add a feature where you prefer to move to locations in the neighborhood. The idea is that a move may be cheaper if the agent didn't move as far. Define a neighborhood however you like.

# todo f. Explore something else you consider interesting.

class Schelling:
    """
    This class implements the Schelling model of segregation.

    Note:
    - self.agent_location_to_color is a dictionary of the form {(x,y): color, ...}
    - self.empty_spaces is a list of tuples (x,y) representing empty spaces
    - self.width and self.height are the dimensions of the grid
    - self.color_to_similarity_thresholds is a dictionary of the form {color: similarity_threshold, ...}
    - self.verbose is a boolean that indicates whether to print or not
    """

    def __init__(self, width=50, height=50, empty_ratio=0.3, similarity_thresholds=None, colors=2, verbose=False):
        if similarity_thresholds is None:
            similarity_thresholds = [0.5, 0.5]
        if len(similarity_thresholds) != colors:
            raise ValueError("The number of similarity thresholds must match the number of colors")
        self.agent_location_to_color = {}
        self.width = width
        self.height = height
        self.colors = colors
        self.color_to_similarity_thresholds = {}
        for color in range(1, colors + 1):
            self.color_to_similarity_thresholds[color] = similarity_thresholds[color - 1]
        self.verbose = verbose
        self.__populate(empty_ratio)

    def __populate(self, empty_ratio):
        self.empty_spaces = []
        all_spaces = list(product(range(self.width), range(self.height)))
        shuffle(all_spaces)
        num_empty = int(empty_ratio * len(all_spaces))
        self.empty_spaces = all_spaces[:num_empty]
        remaining_spaces = all_spaces[num_empty:]
        spaces_by_color = [remaining_spaces[i::self.colors] for i in range(self.colors)]
        for i in range(self.colors):
            # create agents for each color
            self.agent_location_to_color = {**self.agent_location_to_color, **dict(zip(spaces_by_color[i], [i + 1] * len(spaces_by_color[i])))}
        if self.verbose:
            print("Populate ", self.width, self.height)
            print(all_spaces)
            print("Houses by color ", spaces_by_color[0])
            print("dictionary", self.agent_location_to_color)

    def simulate(self, run, num, num_iterations):
        self.__plot(f'Schelling Model with {self.colors} colors: Initial State', f'schelling_{num}_initial.png', run)
        color_to_happiness_percentage = self.__simulate(num_iterations)
        print('Final State')
        for color in color_to_happiness_percentage:
            print(f'\tColor {color} percentage meeting the threshold: {round(color_to_happiness_percentage[color] * 100, 2)}%')
        self.__plot(f'Schelling Model with {self.colors} colors: {num}%', f'schelling_{num}_final.png', run)

    def __plot(self, title, file_name, run):
        fig, ax = plt.subplots()
        # If you want to run the simulation with more than 7 colors, you should set agent_colors accordingly
        agent_colors = {1: 'b', 2: 'r', 3: 'g', 4: 'c', 5: 'm', 6: 'y', 7: 'k'}
        marker_size = 150 / self.width  # no logic here, I just played around with it
        for agent in self.agent_location_to_color:
            ax.scatter(agent[0] + 0.5, agent[1] + 0.5, s=marker_size, color=agent_colors[self.agent_location_to_color[agent]])
        ax.set_title(title, fontsize=10, fontweight='bold')
        ax.set_xlim([0, self.width])
        ax.set_ylim([0, self.height])
        ax.set_xticks([])
        ax.set_yticks([])
        plt.savefig(f"../data/run{run}/{file_name}")

    def __simulate(self, num_iterations):
        if self.verbose:
            for color in self.color_to_similarity_thresholds:
                print(f'Color {color} similarity threshold: {round(self.color_to_similarity_thresholds[color] * 100, 2)}%')
        total_distance = 0
        last_color_to_happiness_percentage = {}
        is_simular = False
        for i in range(num_iterations):
            if is_simular:
                break
            num_changes, total_distance = self.__move_agents(total_distance)
            if i % 30 == 0:
                color_to_happiness_percentage = self.__get_color_to_happiness_percentage()
                if self.verbose:
                    print(f'Iteration: {i + 1}')
                    for color in color_to_happiness_percentage:
                        print(f'\tColor {color} percentage meeting the threshold: {round(color_to_happiness_percentage[color] * 100, 2)}%')
                    print(f'\tTotal distance: {total_distance}')
                    print(f'\tNumber of changes: {num_changes}')

                is_simular, last_color_to_happiness_percentage = self.__check_progress(i, color_to_happiness_percentage, last_color_to_happiness_percentage)
            if num_changes == 0:
                break
        return self.__get_color_to_happiness_percentage()

    def __move_agents(self, total_distance):
        old_agent_location_to_color = deepcopy(self.agent_location_to_color)
        num_changes = 0
        unhappy_agents = []
        for agent in old_agent_location_to_color:
            unhappy_agents.append(agent) if self.__is_unsatisfied(agent) else None
        for agent in unhappy_agents:
            has_near_agent, near_agent = self.unhappy_agent_in_area(agent, unhappy_agents, 4)
            if has_near_agent == "swap near agents":
                num_changes, total_distance = self.swap_agents(agent, near_agent, num_changes, total_distance)
                del unhappy_agents[unhappy_agents.index(near_agent)]
                del unhappy_agents[unhappy_agents.index(agent)]
            elif has_near_agent == "swap with near empty space":
                num_changes, total_distance = self.move_agent_to_empty_space(agent, near_agent, num_changes, total_distance)
                del unhappy_agents[unhappy_agents.index(agent)]
            else:
                num_changes, total_distance = self.check_map_for_swap(agent, unhappy_agents, num_changes, total_distance)
        return num_changes, total_distance

    def check_map_for_swap(self, agent, unhappy_agents, num_changes, total_distance):
        good = False
        if not good:
            for other_agent in unhappy_agents:
                is_other_agent_space_better_for_agent = self.get_number_of_color_by_agent(self.agent_location_to_color[agent], other_agent) > self.get_number_of_color_by_agent(
                    self.agent_location_to_color[agent], agent)
                is_agent_space_better_for_other_agent = self.get_number_of_color_by_agent(self.agent_location_to_color[other_agent], agent) > self.get_number_of_color_by_agent(
                    self.agent_location_to_color[other_agent], other_agent)
                if (other_agent != agent and self.agent_location_to_color[other_agent] != self.agent_location_to_color[agent]
                        and is_other_agent_space_better_for_agent and is_agent_space_better_for_other_agent):
                    num_changes, total_distance = self.swap_agents(agent, other_agent, num_changes, total_distance)
                    del unhappy_agents[unhappy_agents.index(other_agent)]
                    del unhappy_agents[unhappy_agents.index(agent)]
                    good = True
                    break
        if not good:
            for other_agent in self.empty_spaces:
                is_other_agent_space_better_for_agent = self.get_number_of_color_by_agent(self.agent_location_to_color[agent], other_agent) > self.get_number_of_color_by_agent(
                    self.agent_location_to_color[agent], agent)
                if other_agent != agent and is_other_agent_space_better_for_agent:
                    num_changes, total_distance = self.move_agent_to_empty_space(agent, other_agent, num_changes, total_distance)
                    del unhappy_agents[unhappy_agents.index(agent)]
                    good = True
                    break
        if not good:
            num_changes, total_distance = self.move_agent_to_empty_space(agent, choice(self.empty_spaces), num_changes, total_distance)
            del unhappy_agents[unhappy_agents.index(agent)]
        return num_changes, total_distance

    def unhappy_agent_in_area(self, agent, unhappy_agents, area_size):
        x = agent[0]
        y = agent[1]
        # todo make sure the near_agent is better than the current happy score and make sure they are of different colors
        for i in range(-area_size, area_size):
            for j in range(-area_size, area_size):
                other_agent = (x + i, y + j)
                if 0 <= other_agent[0] < self.width and 0 <= other_agent[1] < self.height and other_agent != agent:
                    is_other_agent_space_better_for_agent = self.get_number_of_color_by_agent(self.agent_location_to_color[agent], other_agent) > self.get_number_of_color_by_agent(
                        self.agent_location_to_color[agent], agent)
                    if other_agent in unhappy_agents:
                        is_agent_space_better_for_other_agent = self.get_number_of_color_by_agent(self.agent_location_to_color[other_agent], agent) > self.get_number_of_color_by_agent(
                            self.agent_location_to_color[other_agent], other_agent)
                        if self.agent_location_to_color[other_agent] != self.agent_location_to_color[agent] and is_other_agent_space_better_for_agent and is_agent_space_better_for_other_agent:
                            return "swap near agents", other_agent
                    if other_agent in self.empty_spaces and is_other_agent_space_better_for_agent:
                        return "swap with near empty space", other_agent
        return None, None

    def get_number_of_color_by_agent(self, color_to_look_for, agent):
        if agent in self.agent_location_to_color and self.agent_location_to_color[agent] == color_to_look_for:
            return 0
        x = agent[0]
        y = agent[1]
        neighborhood_size = 1
        count = 0
        for i in range(-neighborhood_size, neighborhood_size):
            for j in range(-neighborhood_size, neighborhood_size):
                other_agent = (x + i, y + j)
                if (0 <= other_agent[0] < self.width and 0 <= other_agent[1] < self.height and other_agent != agent
                        and other_agent in self.agent_location_to_color and color_to_look_for == self.agent_location_to_color[other_agent]):
                    count += 1
        return count

    def swap_agents(self, agent1, agent2, num_changes, total_distance):
        agent1_color = self.agent_location_to_color[agent1]
        agent2_color = self.agent_location_to_color[agent2]
        self.agent_location_to_color[agent1] = agent2_color
        self.agent_location_to_color[agent2] = agent1_color
        total_distance += abs(agent1[0] - agent2[0]) + abs(agent1[1] - agent2[1])
        num_changes += 1
        return num_changes, total_distance

    def move_agent_to_empty_space(self, agent, empty_space, num_changes, total_distance):
        agent_color = self.agent_location_to_color[agent]
        self.agent_location_to_color[empty_space] = agent_color
        del self.agent_location_to_color[agent]
        self.empty_spaces.remove(empty_space)
        self.empty_spaces.append(agent)
        total_distance += abs(empty_space[0] - agent[0]) + abs(empty_space[1] - agent[1])
        num_changes += 1
        return num_changes, total_distance

    def __is_unsatisfied(self, agent):
        num_different, num_similar = self.__get_similar_and_different_count(agent)
        try:
            return float(num_similar) / (num_similar + num_different) < self.color_to_similarity_thresholds[self.agent_location_to_color[agent]]
        except ZeroDivisionError:
            return False

    def __get_color_to_happiness_percentage(self):
        color_to_happiness_percentage = {}
        for color in range(1, self.colors + 1):
            similarity_count = 0
            num_agents = 0
            for agent in self.agent_location_to_color:
                if self.agent_location_to_color[agent] == color:
                    num_agents += 1
                    num_different, num_similar = self.__get_similar_and_different_count(agent)
                    if num_similar + num_different != 0 and float(num_similar) / (num_similar + num_different) >= self.color_to_similarity_thresholds[color]:
                        similarity_count += 1
            color_to_happiness_percentage.update({color: (float(similarity_count) / num_agents)})
        return color_to_happiness_percentage

    @staticmethod
    def __check_progress(i, color_to_happiness_percentage, last_color_to_happiness_percentage):
        is_simular = False
        for color in color_to_happiness_percentage:
            if (color in last_color_to_happiness_percentage and
                    abs(color_to_happiness_percentage[color] - last_color_to_happiness_percentage[color]) <=
                    0.02 * max(color_to_happiness_percentage[color], last_color_to_happiness_percentage[color])):
                print(f'Little progress in color {color} happiness. Iteration: {i + 1}')
                is_simular = True
                break
        last_color_to_happiness_percentage = color_to_happiness_percentage
        return is_simular, last_color_to_happiness_percentage

    def __get_similar_and_different_count(self, agent):
        num_similar = 0
        num_different = 0
        color = self.agent_location_to_color[agent]
        x = agent[0]
        y = agent[1]
        if x > 0 and y > 0 and (x - 1, y - 1) not in self.empty_spaces:
            if self.agent_location_to_color[(x - 1, y - 1)] == color:
                num_similar += 1
            else:
                num_different += 1
        if y > 0 and (x, y - 1) not in self.empty_spaces:
            if self.agent_location_to_color[(x, y - 1)] == color:
                num_similar += 1
            else:
                num_different += 1
        if x < (self.width - 1) and y > 0 and (x + 1, y - 1) not in self.empty_spaces:
            if self.agent_location_to_color[(x + 1, y - 1)] == color:
                num_similar += 1
            else:
                num_different += 1
        if x > 0 and (x - 1, y) not in self.empty_spaces:
            if self.agent_location_to_color[(x - 1, y)] == color:
                num_similar += 1
            else:
                num_different += 1
        if x < (self.width - 1) and (x + 1, y) not in self.empty_spaces:
            if self.agent_location_to_color[(x + 1, y)] == color:
                num_similar += 1
            else:
                num_different += 1
        if x > 0 and y < (self.height - 1) and (x - 1, y + 1) not in self.empty_spaces:
            if self.agent_location_to_color[(x - 1, y + 1)] == color:
                num_similar += 1
            else:
                num_different += 1
        if x > 0 and y < (self.height - 1) and (x, y + 1) not in self.empty_spaces:
            if self.agent_location_to_color[(x, y + 1)] == color:
                num_similar += 1
            else:
                num_different += 1
        if x < (self.width - 1) and y < (self.height - 1) and (x + 1, y + 1) not in self.empty_spaces:
            if self.agent_location_to_color[(x + 1, y + 1)] == color:
                num_similar += 1
            else:
                num_different += 1
        return num_different, num_similar
