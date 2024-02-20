# Nate Stott A02386053
# CS 5110 - Vicki Allan - Program3 Normal Form Games
# 02/19/2024


from random import randint, shuffle


def main():
    for game_number in range(1, 4):
        print()
        print("*" * 50)
        print(f"Game {game_number}:")
        game = Game(game_number)
        game.play()


class Player:

    def __init__(self, title: str, index: int, number_of_options: int, rewards: list[int]):

        self.title: str = title
        self.index: int = index
        self.number_of_options: int = number_of_options
        self.rewards: list[int] = rewards
        self.first_options: list[list[int]] = self.__get_first_options()
        self.second_options: list[list[int]] = self.__get_second_options()
        self.strongly_dominated_strategies: int = self.__get_strongly_dominated_strategy()
        self.weakly_dominated_strategies: list[int] = list(set(self.__get_weakly_dominated_strategies()))

        self.second_option_choice_locations = self.__get_second_option_choice_locations()

        self.nash_equilibria_options: list[int] = []
        self.pareto_optimal_options: list[int] = []

        self.strategy_titles = [
            "pure_strategy",
            "minimax_strategy",
            "maximin_strategy",
            "nash_equilibrium_strategy",
            "pareto_optimal_strategy",
            "weakly_dominated_strategy",
            "strongly_dominated_strategy"
        ]

        self.strategy = None

    def set_strategy(self, strategy_title: str):
        if strategy_title not in self.strategy_titles:
            raise ValueError(f"Invalid strategy: {strategy_title}")
        self.strategy = getattr(self, strategy_title)
    def set_nash_equilibria_options(self, nash_equilibria_locations):
        self.nash_equilibria_options = list(set(self.__get_options(nash_equilibria_locations)))

    def set_pareto_optimal_options(self, pareto_optimal_locations):
        self.pareto_optimal_options = list(set(self.__get_options(pareto_optimal_locations)))

    def __get_second_option_choice_locations(self):
        choice_locations = []
        for i in range(len(self.second_options)):
            choice_locations.append(self.__get_best_choices(self.second_options[i], i))
        return choice_locations

    def __get_best_choices(self, options, i):
        best_choices = []
        best_reward = 0
        for j in range(len(options)):
            if options[j] > best_reward:
                best_reward = options[j]
        for j in range(len(options)):
            if options[j] == best_reward:
                if self.index == 0:
                    best_choices.append((j, i))
                else:
                    best_choices.append((i, j))
        return best_choices

    def __get_strongly_dominated_strategy(self):

        i: int = 0
        j: int = i + 1

        is_strongly_dominated: bool = False

        while j < len(self.first_options):
            option1: list[int] = self.first_options[i]
            option2: list[int] = self.first_options[j]
            option1_count: int = 0
            option2_count: int = 0
            for k in range(len(option1)):
                if option1[k] > option2[k]:
                    option1_count += 1
                elif option2[k] > option1[k]:
                    option2_count += 1
            if option1_count > option2_count and option1_count == len(option1):
                j += 1
                is_strongly_dominated = True
            elif option2_count > option1_count and option2_count == len(option2):
                i = j
                j += 1
                is_strongly_dominated = True
            else:
                i += 1
                j = i + 1
                is_strongly_dominated = False
        if is_strongly_dominated:
            return i
        return None

    def __get_weakly_dominated_strategies(self):
        weakly_dominated_strategies = []
        i: int = 0
        j: int = i + 1
        while j < len(self.first_options):
            temps = []
            option1, option1_count, option2, option2_count = self.__get_weakly_counts(i, j)
            if option1_count > option2_count and option1_count == len(option1):
                temps.append(i)
                j += 1
            elif option2_count > option1_count and option2_count == len(option2):
                temps.append(j)
                i = j
                j += 1
            elif option1_count == option2_count and option1_count == len(option1) and option2_count == len(option2):
                temps.append(i)
                temps.append(j)
                i += 1
                j = i + 1
            else:
                i += 1
                j = i + 1
            if len(weakly_dominated_strategies) == 0:
                weakly_dominated_strategies = temps
            else:
                other_temps = weakly_dominated_strategies.copy()
                for temp in temps:
                    for other in other_temps:
                        option1, option1_count, option2, option2_count = self.__get_weakly_counts(temp, other)
                        if option1_count > option2_count and option1_count == len(option1):
                            weakly_dominated_strategies.append(temp)
                            weakly_dominated_strategies.remove(other)
                        elif option1_count == option2_count and option1_count == len(option1) and option2_count == len(option2):
                            weakly_dominated_strategies.append(temp)
        return weakly_dominated_strategies

    def __get_weakly_counts(self, i, j):
        option1: list[int] = self.first_options[i]
        option2: list[int] = self.first_options[j]
        option1_count: int = 0
        option2_count: int = 0
        for k in range(len(option1)):
            if option1[k] > option2[k]:
                option1_count += 1
            elif option2[k] > option1[k]:
                option2_count += 1
            elif option1[k] == option2[k]:
                option1_count += 1
                option2_count += 1
        return option1, option1_count, option2, option2_count

    def __get_first_options(self):
        first_options = []
        if self.index == 0:
            cut = (len(self.rewards) // self.number_of_options)
            for i in range(0, len(self.rewards), cut):
                first_options.append(self.rewards[i:i + cut])
        else:
            row = []
            for i in range(0, len(self.rewards), self.number_of_options):
                row.append(self.rewards[i:i + self.number_of_options])
            for i in range(len(row[0])):
                first_options.append([])
                for j in range(len(row)):
                    first_options[i].append(row[j][i])
        return first_options

    def __get_second_options(self):
        second_options = [[] for _ in range(len(self.first_options[0]))]
        for i in range(len(self.first_options)):
            for j in range(len(self.first_options[i])):
                second_options[j].append(self.first_options[i][j])
        return second_options

    def __get_options(self, locations: tuple[tuple[int, int], ...]):
        return [location[self.index] for location in locations]

    @staticmethod
    def __best_strategy(options: list[int]):
        best_reward = 0
        best_index = 0
        for j in range(len(options)):
            if options[j] > best_reward:
                best_reward = options[j]
                best_index = j
        return best_index

    def minimax_strategy(self, last_choice: int):
        if last_choice is None:
            return self.__minimax()
        return self.__best_strategy(self.second_options[last_choice])

    def __minimax(self):
        regrets = [[] for _ in range(self.number_of_options)]
        for options in self.second_options:
            largest = max(options)
            for i in range(len(options)):
                option = options[i]
                regrets[i].append(largest - option)
        lowest_regret = sum(regrets[0])
        lowest_regret_index = 0
        for i in range(1, len(regrets)):
            regret = sum(regrets[i])
            if regret < lowest_regret:
                lowest_regret = regret
                lowest_regret_index = i
        return lowest_regret_index

    def maximin_strategy(self, last_choice: int):
        if last_choice is None:
            return self.__maximin()
        return self.__best_strategy(self.second_options[last_choice])

    def __maximin(self):
        largest_minimum = min(self.first_options[0])
        largest_minimum_index = 0
        for i in range(1, len(self.first_options)):
            minimum = min(self.first_options[i])
            if minimum > largest_minimum:
                largest_minimum = minimum
                largest_minimum_index = i
        return largest_minimum_index

    def pure_strategy(self, last_choice: int = None):
        if last_choice is None:
            return randint(0, self.number_of_options - 1)
        return self.__best_strategy(self.second_options[last_choice])

    def nash_equilibrium_strategy(self, last_choice: int):
        if last_choice is None:
            if not self.nash_equilibria_options:
                return randint(0, self.number_of_options - 1)
            shuffle(self.nash_equilibria_options)
            return self.nash_equilibria_options[0]
        return self.__best_strategy(self.second_options[last_choice])

    def pareto_optimal_strategy(self, last_choice: int):
        if last_choice is None:
            if not self.pareto_optimal_options:
                return self.pure_strategy()
            shuffle(self.pareto_optimal_options)
            return self.pareto_optimal_options[0]
        return self.__best_strategy(self.second_options[last_choice])

    def strongly_dominated_strategy(self, last_choice: int):
        if last_choice is None:
            if self.strongly_dominated_strategies is None:
                return self.pure_strategy()
            return self.strongly_dominated_strategies
        return self.__best_strategy(self.second_options[last_choice])

    def weakly_dominated_strategy(self, last_choice: int):
        if last_choice is None:
            if not self.weakly_dominated_strategies:
                return self.pure_strategy()
            shuffle(self.weakly_dominated_strategies)
            return self.weakly_dominated_strategies[0]
        return self.__best_strategy(self.second_options[last_choice])


class Game:

    def __init__(self, game: int):

        game_table, row_player_rewards, col_player_rewards, row_player_size, col_player_size = self.get_game_table(
            f"../Data/game_{game}.txt")
        self.game_table = game_table
        self.game_title = f"Game {game}"

        self.row_player = Player("row player", 0, row_player_size, row_player_rewards)
        self.column_player = Player("column player", 1, col_player_size, col_player_rewards)
        self.nash_equilibria_locations = self.get_nash_equilibria_locations()
        self.pareto_optimal_locations = self.get_pareto_optimal_locations()
        self.row_player.set_nash_equilibria_options(self.nash_equilibria_locations)
        self.row_player.set_pareto_optimal_options(self.pareto_optimal_locations)
        self.column_player.set_nash_equilibria_options(self.nash_equilibria_locations)
        self.column_player.set_pareto_optimal_options(self.pareto_optimal_locations)

    def get_nash_equilibria_locations(self) -> tuple[tuple[int, int]]:
        nash_equilibria_locations = []
        for row_choices in self.row_player.second_option_choice_locations:
            for col_choices in self.column_player.second_option_choice_locations:
                for row_choice in row_choices:
                    for col_choice in col_choices:
                        if row_choice == col_choice:
                            nash_equilibria_locations.append(row_choice)
        return tuple(nash_equilibria_locations)

    def get_pareto_optimal_locations(self) -> tuple[tuple[int, int]]:
        pareto_optimal_locations = []
        for i in range(len(self.game_table)):
            for j in range(len(self.game_table[i])):
                cell = self.game_table[i][j]
                if self.is_pareto_optimal(cell):
                    pareto_optimal_locations.append((i, j))
        return tuple(pareto_optimal_locations)

    def is_pareto_optimal(self, cell: tuple[int, int]):
        for row in self.game_table:
            for other_cell in row:
                if cell[0] < other_cell[0] and cell[1] < other_cell[1]:
                    return False
        return True

    def play(self):
        self.print_game_table()
        print(f"Nash Equilibria Locations: {self.nash_equilibria_locations}")
        print(f"Pareto Optimal Locations: {self.pareto_optimal_locations}")
        print(f"Row Player Strongly Dominated Strategy: {self.row_player.strongly_dominated_strategies}")
        print(f"Column Player Strongly Dominated Strategy: {self.column_player.strongly_dominated_strategies}")
        print(f"Row Player Weakly Dominated Strategies: {self.row_player.weakly_dominated_strategies}")
        print(f"Column Player Weakly Dominated Strategies: {self.column_player.weakly_dominated_strategies}")
        self.run_games()

    def run_games(self):
        print()
        for row_strategy_title in self.row_player.strategy_titles:
            for col_strategy_title in self.column_player.strategy_titles:
                self.run_game(row_strategy_title, col_strategy_title)

    def run_game(self, row_strategy_title: str, column_strategy_title: str):
        self.row_player.set_strategy(row_strategy_title)
        self.column_player.set_strategy(column_strategy_title)
        print(f"Row Player: {row_strategy_title} vs Column Player: {column_strategy_title}")
        self.round(self.row_player, self.column_player)
        print()
        print(f"Column Player: {column_strategy_title} vs Row Player: {row_strategy_title}")
        self.round(self.column_player, self.row_player)
        print()
        self.row_player.strategy = None
        self.column_player.strategy = None

    def round(self, first_player, second_player):
        first_player_choice = first_player.strategy(None)
        second_player_choice = second_player.strategy(first_player_choice)
        if first_player.index == 0:
            player_rewards = self.game_table[first_player_choice][second_player_choice]
        else:
            player_rewards = self.game_table[second_player_choice][first_player_choice]
        first_player_reward = player_rewards[first_player.index]
        second_player_reward = player_rewards[second_player.index]
        first_player_title = first_player.title
        second_player_title = second_player.title
        print(f"\t{first_player_title} chose:", first_player_choice)
        print(f"\t{second_player_title} chose:", second_player_choice)
        print(f"\t{first_player_title} reward:", first_player_reward)
        print(f"\t{second_player_title} reward:", second_player_reward)

    def print_game_table(self):
        max_cell_length = 0
        for i in range(len(self.game_table)):
            for j in range(len(self.game_table[i])):
                cell_length = len(str(self.game_table[i][j]).strip("(").strip(")"))
                if cell_length > max_cell_length:
                    max_cell_length = cell_length
        max_cell_length += 2
        row_length = (max_cell_length + 3) * len(self.game_table[0]) + 1
        print("-" * row_length)
        spaces = " " * max_cell_length
        print(f"  |", end="")
        for i in range(len(self.game_table[0])):
            this_spaces = " " * (len(spaces) - len(str(i)))
            print(f"{this_spaces}{i} |", end="")
        print()
        print("-" * row_length)
        for i in range(len(self.game_table)):
            start = f"{i} |"
            print(start, end="")
            for j in range(len(self.game_table[i])):
                cell = str(self.game_table[i][j]).strip("(").strip(")")
                this_spaces = " " * (len(spaces) - len(cell))
                print(f"{this_spaces}{cell} |", end="")
            print()
            print("-" * row_length)
        print()

    def get_game_table(self, file_path: str):
        data = self.get_info_from_game_file(file_path)
        if not data:
            return None
        row_player_size, col_player_size, row_player_rewards, col_player_rewards = data
        game_table = []
        for i in range(row_player_size):
            game_table.append([])
            for j in range(i * col_player_size, (i + 1) * col_player_size):
                game_table[i].append((row_player_rewards[j], col_player_rewards[j]))
            game_table[i] = tuple(game_table[i])
        return tuple(game_table), row_player_rewards, col_player_rewards, row_player_size, col_player_size

    @staticmethod
    def get_info_from_game_file(file_path: str):
        try:
            with open(file_path, "r") as file:
                sizes = file.readline().split(" ")
                row_player_size = int(sizes[0])
                col_player_size = int(sizes[1])
                row_player_rewards = [int(x) for x in file.readline().split(" ")]
                col_player_rewards = [int(x) for x in file.readline().split(" ")]
        except FileNotFoundError:
            print("File not found")
            return None
        except ValueError:
            print("File is not formatted correctly")
            return None
        except Exception as e:
            print("An error occurred:", e)
            return None
        if not row_player_size or not col_player_size or not row_player_rewards or not col_player_rewards:
            print("File is not formatted correctly")
            return None
        return row_player_size, col_player_size, row_player_rewards, col_player_rewards


if __name__ == "__main__":
    main()
