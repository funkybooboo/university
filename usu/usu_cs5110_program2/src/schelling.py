import matplotlib.pyplot as plt
from itertools import product
from random import shuffle, choice, randint
from copy import deepcopy
from os import mkdir, path


# a. Add output to indicate the percentage of each agent type that meets the desired similarity_threshold.

# b. Add a feature where the similarity_threshold can be different for each of the colors.

# c. Add a feature to stop when little progress is being made. Print out a message to indicate how many iterations you did.

# d. Add a feature where agents can improve on their current location by performing a location swap with another agent who is willing to swap (in addition to just swapping with an open position).

# e. Add a feature where you prefer to move to locations in the neighborhood. The idea is that a move may be cheaper if the agent didn't move as far. Define a neighborhood however you like.

# f. Explore something else you consider interesting.
# Each color of agent would have to specify their wantness with each other color of agent the total has to add up to 1.
# If in a list [.5, .2, .3] meaning they want 50% of the first color, 20% of the second color, and 30% of the third color.
# [.5, 0] meaning they want 50% of the first color and 0% of the second color.

class Schelling:
    """
    This class implements the Schelling model of segregation.

    Note:
    self.__width and self.__height are the dimensions of the grid.
    self.__agent_location_to_color is a dictionary mapping the location of an agent to its color.
    self.__color_to_similarity_thresholds is a dictionary mapping a color to a list of similarity thresholds.
    self.__empty_spaces is a list of empty spaces on the grid.
    self.__colors is the number of colors.
    self.__verbose is a boolean indicating whether to print out information about the simulation.
    """

    def __init__(self, width=50, height=50, empty_ratio=0.3, similarity_thresholds=None, colors=2, verbose=False):
        """
        Args:
            width: The width of the grid.
            height: The height of the grid.
            empty_ratio: How much of the grid should be empty.
            similarity_thresholds: A dictionary mapping a color to a list of similarity thresholds.
            colors: The number of colors.
            verbose: Flag to indicate whether to print out information about the simulation.
        """
        if verbose:
            print("Width: ", width, " Height: ", height, " Empty Ratio: ", empty_ratio, " Colors: ", colors, " Similarity Thresholds: ", similarity_thresholds, " Verbose: ", verbose)
        if similarity_thresholds is None:
            similarity_thresholds = [[0.5, 0], [0, 0.5]]
        if len(similarity_thresholds) != colors:
            raise ValueError("The number of similarity threshold lists must match the number of colors")
        self.__agent_location_to_color = {}
        self.__width = width
        self.__height = height
        self.__colors = colors
        self.__color_to_similarity_thresholds = {}
        for color1 in range(1, colors + 1):
            total = 0
            try:
                for color2 in range(1, colors + 1):
                    total += similarity_thresholds[color1 - 1][color2 - 1]
                if total > 1 or total < 0:
                    raise ValueError("The sum of the similarity thresholds for each color must be between 0 and 1")
            except TypeError:
                raise ValueError("The sum of the similarity thresholds for each color must be between 0 and 1")
            self.__color_to_similarity_thresholds[color1] = similarity_thresholds[color1 - 1]
        self.__verbose = verbose
        self.__populate(empty_ratio)

    def __populate(self, empty_ratio):
        self.empty_spaces = []
        all_spaces = list(product(range(self.__width), range(self.__height)))
        shuffle(all_spaces)
        num_empty = int(empty_ratio * len(all_spaces))
        self.empty_spaces = all_spaces[:num_empty]
        remaining_spaces = all_spaces[num_empty:]
        spaces_by_color = [remaining_spaces[i::self.__colors] for i in range(self.__colors)]
        for i in range(self.__colors):
            # create agents for each color
            self.__agent_location_to_color = {**self.__agent_location_to_color, **dict(zip(spaces_by_color[i], [i + 1] * len(spaces_by_color[i])))}

    @staticmethod
    def run_random_simulation(what_run, what_simulation_for_this_run, verbose=False):
        """
        This method runs a random simulation.
        Returns: None

        """
        width = randint(10, 50)
        height = randint(10, 50)
        empty_ratio = randint(0, 100) / 100
        colors = randint(1, 7)
        similarity_thresholds = []
        for i in range(colors):
            similarity_thresholds.append([])
            total = 0
            for j in range(colors):
                r = randint(0, 100 - total)
                total += r
                similarity_thresholds[i].append(round(r / 100, 2))
        simulation = Schelling(width, height, empty_ratio, similarity_thresholds, colors, verbose)
        simulation.simulate(what_run, what_simulation_for_this_run, 200)

    def simulate(self, what_run, what_simulation_for_this_run, num_iterations):
        """
        Args:
            what_run: Which run this is.
            what_simulation_for_this_run: Which simulation this is, in the run.
            num_iterations: How many iterations to run the simulation for.

        Returns: None
        """
        self.__create_save_location(what_run)
        self.__plot(f'Schelling Model with {self.__colors} colors: Initial State', f'schelling_{what_simulation_for_this_run}_initial.png', what_run)
        color_to_happiness_percentage = self.__simulate(num_iterations)
        if self.__verbose:
            print('Final State')
            for color in color_to_happiness_percentage:
                print(f'\tColor {color} percentage meeting the threshold: {round(color_to_happiness_percentage[color] * 100, 2)}%')
        self.__plot(f'Schelling Model with {self.__colors} colors: {what_simulation_for_this_run}%', f'schelling_{what_simulation_for_this_run}_final.png', what_run)

    @staticmethod
    def __create_save_location(run):
        data_path = "../data"
        try:
            mkdir(data_path)
        except FileExistsError:
            pass
        run_path = path.join("../data/", f"run{run}")
        try:
            mkdir(run_path)
        except FileExistsError:
            pass

    def __plot(self, title, file_name, what_run):
        """
        Args:
            title: title of the plot
            file_name: title of the file
            what_run: gives information about where to save the file

        Returns: None

        """
        fig, ax = plt.subplots()
        # If you want to run the simulation with more than 7 colors, you should set agent_colors accordingly
        agent_colors = {1: 'b', 2: 'r', 3: 'g', 4: 'c', 5: 'm', 6: 'y', 7: 'k'}
        marker_size = 150 / self.__width  # no logic here, I just played around with it
        for agent in self.__agent_location_to_color:
            ax.scatter(agent[0] + 0.5, agent[1] + 0.5, s=marker_size, color=agent_colors[self.__agent_location_to_color[agent]])
        ax.set_title(title, fontsize=10, fontweight='bold')
        ax.set_xlim([0, self.__width])
        ax.set_ylim([0, self.__height])
        ax.set_xticks([])
        ax.set_yticks([])
        plt.savefig(f"../data/run{what_run}/{file_name}")

    def __simulate(self, num_iterations):
        """
        Args:
            num_iterations: number of iterations to run the simulation for

        Returns: a dictionary mapping a color to the percentage of agents of that color that meet the similarity threshold

        """
        if self.__verbose:
            for color in self.__color_to_similarity_thresholds:
                print(f'Color {color} similarity thresholds:')
                for i in range(self.__colors):
                    if i + 1 != color:
                        print(f'\tFor color {i + 1}: {self.__color_to_similarity_thresholds[color][i] * 100}%')
                    else:
                        print(f'\tFor its own color: {self.__color_to_similarity_thresholds[color][i] * 100}%')
        total_distance = 0
        last_color_to_happiness_percentage = {}
        is_simular = False
        for i in range(num_iterations):
            if is_simular:
                break
            num_changes, total_distance = self.__move_agents(total_distance)
            if i % 30 == 0:
                color_to_happiness_percentage = self.__get_color_to_happiness_percentage()
                if self.__verbose:
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
        """

        Args:
            total_distance: tells how far the agents have moved

        Returns: number of changes made and total distance across the grid

        """
        old_agent_location_to_color = deepcopy(self.__agent_location_to_color)
        num_changes = 0
        unhappy_agents = []
        for agent in old_agent_location_to_color:
            if self.__is_unsatisfied(agent):
                unhappy_agents.append(agent)
        for agent in unhappy_agents:
            has_near_agent, near_agent = self.__unhappy_agent_in_area(agent, unhappy_agents, 4)
            if has_near_agent == "swap near agents":
                num_changes, total_distance = self.__swap_agents(agent, near_agent, num_changes, total_distance)
                del unhappy_agents[unhappy_agents.index(near_agent)]
                del unhappy_agents[unhappy_agents.index(agent)]
            elif has_near_agent == "swap with near empty space":
                num_changes, total_distance = self.__move_agent_to_empty_space(agent, near_agent, num_changes, total_distance)
                del unhappy_agents[unhappy_agents.index(agent)]
            else:
                num_changes, total_distance = self.__check_map_for_swap(agent, unhappy_agents, num_changes, total_distance)
        return num_changes, total_distance

    def __check_map_for_swap(self, agent, unhappy_agents, num_changes, total_distance):
        """

        Args:
            agent: which agent to check
            unhappy_agents: all the unhappy agents
            num_changes: how many changes have been made
            total_distance: how far the agents have moved

        Returns: number of changes made and total distance across the grid

        """
        good = False
        if not good:
            for other_agent in unhappy_agents:
                is_other_agent_space_better_for_agent = self.__get_number_of_color_by_agent(self.__agent_location_to_color[agent], other_agent) > self.__get_number_of_color_by_agent(
                    self.__agent_location_to_color[agent], agent)
                is_agent_space_better_for_other_agent = self.__get_number_of_color_by_agent(self.__agent_location_to_color[other_agent], agent) > self.__get_number_of_color_by_agent(
                    self.__agent_location_to_color[other_agent], other_agent)
                if (other_agent != agent and self.__agent_location_to_color[other_agent] != self.__agent_location_to_color[agent]
                        and is_other_agent_space_better_for_agent and is_agent_space_better_for_other_agent):
                    num_changes, total_distance = self.__swap_agents(agent, other_agent, num_changes, total_distance)
                    del unhappy_agents[unhappy_agents.index(other_agent)]
                    del unhappy_agents[unhappy_agents.index(agent)]
                    good = True
                    break
        if not good:
            for other_agent in self.empty_spaces:
                is_other_agent_space_better_for_agent = self.__get_number_of_color_by_agent(self.__agent_location_to_color[agent], other_agent) > self.__get_number_of_color_by_agent(
                    self.__agent_location_to_color[agent], agent)
                if other_agent != agent and is_other_agent_space_better_for_agent:
                    num_changes, total_distance = self.__move_agent_to_empty_space(agent, other_agent, num_changes, total_distance)
                    del unhappy_agents[unhappy_agents.index(agent)]
                    good = True
                    break
        if not good:
            num_changes, total_distance = self.__move_agent_to_empty_space(agent, choice(self.empty_spaces), num_changes, total_distance)
            del unhappy_agents[unhappy_agents.index(agent)]
        return num_changes, total_distance

    def __unhappy_agent_in_area(self, agent, unhappy_agents, area_size):
        """

        Args:
            agent: which agent to check
            unhappy_agents: all the unhappy agents
            area_size: how big of an area to check

        Returns: what type of agent is in the area and the agent

        """
        x = agent[0]
        y = agent[1]
        for i in range(-area_size, area_size):
            for j in range(-area_size, area_size):
                other_agent = (x + i, y + j)
                if 0 <= other_agent[0] < self.__width and 0 <= other_agent[1] < self.__height and other_agent != agent:
                    is_other_agent_space_better_for_agent = self.__get_number_of_color_by_agent(self.__agent_location_to_color[agent], other_agent) > self.__get_number_of_color_by_agent(
                        self.__agent_location_to_color[agent], agent)
                    if other_agent in unhappy_agents:
                        is_agent_space_better_for_other_agent = self.__get_number_of_color_by_agent(self.__agent_location_to_color[other_agent], agent) > self.__get_number_of_color_by_agent(
                            self.__agent_location_to_color[other_agent], other_agent)
                        if self.__agent_location_to_color[other_agent] != self.__agent_location_to_color[agent] and is_other_agent_space_better_for_agent and is_agent_space_better_for_other_agent:
                            return "swap near agents", other_agent
                    if other_agent in self.empty_spaces and is_other_agent_space_better_for_agent:
                        return "swap with near empty space", other_agent
        return None, None

    def __get_number_of_color_by_agent(self, color_to_look_for, agent):
        """

        Args:
            color_to_look_for: the color to look for by the agent
            agent: which agent to check

        Returns: the number of agents, of the color by the agent

        """
        if agent in self.__agent_location_to_color and self.__agent_location_to_color[agent] == color_to_look_for:
            return 0
        x = agent[0]
        y = agent[1]
        neighborhood_size = 1
        count = 0
        for i in range(-neighborhood_size, neighborhood_size):
            for j in range(-neighborhood_size, neighborhood_size):
                other_agent = (x + i, y + j)
                if (0 <= other_agent[0] < self.__width and 0 <= other_agent[1] < self.__height and other_agent != agent
                        and other_agent in self.__agent_location_to_color and color_to_look_for == self.__agent_location_to_color[other_agent]):
                    count += 1
        return count

    def __swap_agents(self, agent1, agent2, num_changes, total_distance):
        """

        Args:
            agent1: which agent to swap
            agent2: which agent to swap
            num_changes: the number of changes made
            total_distance: the total distance across the grid

        Returns: number of changes made and total distance across the grid

        """
        agent1_color = self.__agent_location_to_color[agent1]
        agent2_color = self.__agent_location_to_color[agent2]
        self.__agent_location_to_color[agent1] = agent2_color
        self.__agent_location_to_color[agent2] = agent1_color
        total_distance += abs(agent1[0] - agent2[0]) + abs(agent1[1] - agent2[1])
        num_changes += 1
        return num_changes, total_distance

    def __move_agent_to_empty_space(self, agent, empty_space, num_changes, total_distance):
        """

        Args:
            agent: which agent to move
            empty_space: which empty space to move to
            num_changes: the number of changes made
            total_distance: the total distance across the grid

        Returns: the number of changes made and total distance across the grid

        """
        agent_color = self.__agent_location_to_color[agent]
        self.__agent_location_to_color[empty_space] = agent_color
        del self.__agent_location_to_color[agent]
        self.empty_spaces.remove(empty_space)
        self.empty_spaces.append(agent)
        total_distance += abs(empty_space[0] - agent[0]) + abs(empty_space[1] - agent[1])
        num_changes += 1
        return num_changes, total_distance

    def __is_unsatisfied(self, agent):
        """

        Args:
            agent: which agent to check

        Returns: whether the agent is unsatisfied

        """
        agent_color = self.__agent_location_to_color[agent]
        neighbors = self.__get_neighbors(agent)
        number_of_neighbors = sum(neighbors)
        number_of_zeros = 0
        for color in range(self.__colors):
            if neighbors[color] == 0:
                number_of_zeros += 1
        if number_of_zeros == self.__colors and number_of_neighbors == 0:
            return False
        if number_of_zeros != self.__colors and number_of_neighbors == 0:
            return True
        for color in range(self.__colors):
            if float(neighbors[color]) / number_of_neighbors < self.__color_to_similarity_thresholds[agent_color][color]:
                return True

    def __get_color_to_happiness_percentage(self):
        """

        Returns: a dictionary mapping a color to the percentage of agents, of that color that meet the similarity threshold

        """
        color_to_happiness_percentage = {}
        for color in range(1, self.__colors + 1):
            happiness_count = 0
            num_agents = 0
            for agent in self.__agent_location_to_color:
                if self.__agent_location_to_color[agent] == color:
                    num_agents += 1
                    if not self.__is_unsatisfied(agent):
                        happiness_count += 1
            color_to_happiness_percentage[color] = float(happiness_count) / num_agents
        return color_to_happiness_percentage

    def __check_progress(self, i, color_to_happiness_percentage, last_color_to_happiness_percentage):
        """

        Args:
            i: what iteration the simulation is on
            color_to_happiness_percentage: a dictionary mapping a color to the percentage of agents, of that color that meet the similarity threshold
            last_color_to_happiness_percentage: the last dictionary mapping a color to the percentage of agents, of that color that meet the similarity threshold

        Returns: whether the simulation is making little progress and the last dictionary mapping a color to the percentage of agents, of that color that meet the similarity threshold

        """
        is_simular = False
        for color in color_to_happiness_percentage:
            if (color in last_color_to_happiness_percentage and
                    abs(color_to_happiness_percentage[color] - last_color_to_happiness_percentage[color]) <=
                    0.05 * max(color_to_happiness_percentage[color], last_color_to_happiness_percentage[color])):
                if self.__verbose:
                    print(f'Little progress in color {color} happiness. Iteration: {i + 1}')
                is_simular = True
                break
        last_color_to_happiness_percentage = color_to_happiness_percentage
        return is_simular, last_color_to_happiness_percentage

    def __get_neighbors(self, agent):
        """

        Args:
            agent: the agent to look around

        Returns: a list of the number of neighbors of each color

        """
        x = agent[0]
        y = agent[1]
        neighborhood_size = 1
        neighbors = []
        for color in range(self.__colors):
            neighbors.append(0)
        for i in range(-neighborhood_size, neighborhood_size):
            for j in range(-neighborhood_size, neighborhood_size):
                other_agent = (x + i, y + j)
                if 0 <= other_agent[0] < self.__width and 0 <= other_agent[1] < self.__height and other_agent != agent and other_agent in self.__agent_location_to_color:
                    neighbors[self.__agent_location_to_color[other_agent] - 1] += 1
        return neighbors
