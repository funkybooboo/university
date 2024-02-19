# Nate Stott A02386053
# CS 5110 - Vicki Allan - Program3 Normal Form Games
# 02/19/2024


from random import randint, shuffle


class Player:

    def __init__(self, title: str, index: int, number_of_options: int, rewards: list[int], nash_equilibria_locations: list[tuple[int, int]], pareto_optimal_locations: list[tuple[int, int]]):
        self.title: str = title
        self.index: int = index
        self.number_of_options: int = number_of_options
        self.rewards: list[int] = rewards
        self.first_options: list[list[int]] = self.__get_first_options()
        self.second_options: list[list[int]] = self.__get_second_options()
        self.nash_equilibria_options: list[int] = self.__get_options(nash_equilibria_locations)
        self.pareto_optimal_options: list[int] = self.__get_options(pareto_optimal_locations)
        self.strongly_dominated_strategy: int = self.__get_strongly_dominated_strategy()
        self.weakly_dominated_strategies: list[int] = self.__get_weakly_dominated_strategies()

        self.strategies = [
            "minimax_strategy",
            "maximin_strategy",
            "mixed_strategy",
            "pure_strategy",
            "nash_equilibrium_strategy",
            "strongly_dominated_strategy",
            "weakly_dominated_strategy",
            "pareto_optimal_strategy"
        ]

    def __get_strongly_dominated_strategy(self):
        return 0

    def __get_weakly_dominated_strategies(self):
        return []

    def __get_first_options(self):
        first_options = []
        cut = (len(self.rewards) // 2) - 1
        for i in range(0, len(self.rewards), cut):
            first_options.append(self.rewards[i:i+cut])
        return first_options

    def __get_second_options(self):
        second_options = [[] for _ in range(len(self.first_options[0]))]
        for i in range(len(self.first_options)):
            for j in range(len(self.first_options[i])):
                second_options[j].append(self.first_options[i][j])
        return second_options

    def __get_options(self, locations: list[tuple[int, int]]):
        return [location[self.index] for location in locations]

    def __best_move(self, last_choice: int):
        return self.__best_strategy(self.first_options[last_choice] if self.index == 0 else self.second_options[last_choice])

    @staticmethod
    def __best_strategy(options: list[int]):
        m = 0
        i = 0
        for j in range(len(options)):
            if options[j] > m:
                m = options[j]
                i = j
        return i

    def minimax_strategy(self, last_choice: int):
        if last_choice is None:
            return 0
        return self.__best_move(last_choice)

    def maximin_strategy(self, last_choice: int):
        if last_choice is None:
            return 0
        return self.__best_move(last_choice)

    def mixed_strategy(self, last_choice: int):
        if last_choice is None:
            return 0
        return self.__best_move(last_choice)

    def pure_strategy(self, last_choice: int):
        if last_choice is None:
            return randint(0, self.number_of_options - 1)
        return self.__best_move(last_choice)

    def nash_equilibrium_strategy(self, last_choice: int):
        if last_choice is None:
            return shuffle(self.nash_equilibria_options)[0]
        return self.__best_move(last_choice)

    def pareto_optimal_strategy(self, last_choice: int):
        if last_choice is None:
            return shuffle(self.pareto_optimal_options)[0]
        return self.__best_move(last_choice)

    def strongly_dominated_strategy(self, last_choice: int):
        if last_choice is None:
            return self.strongly_dominated_strategy
        return self.__best_move(last_choice)

    def weakly_dominated_strategy(self, last_choice: int):
        if last_choice is None:
            return shuffle(self.weakly_dominated_strategies)[0]
        return self.__best_move(last_choice)


class Game:

    @staticmethod
    def main():
        for game_number in range(1, 4):
            print()
            print("*" * 50)
            print(f"Game {game_number}:")
            game = Game(game_number)
            game.play()

    def __init__(self, game: int):

        game_table, row_player_rewards, col_player_rewards, row_player_size, col_player_size = self.get_game_table(f"../data/game_{game}.txt")

        nash_equilibria_locations = self.get_nash_equilibria_locations()
        pareto_optimal_locations = self.get_pareto_optimal_locations()

        self.game_table = game_table
        self.game_title = f"Game {game}"
        self.row_player = Player("row player", 0, row_player_size, row_player_rewards, nash_equilibria_locations)
        self.column_player = Player("column player", 1, col_player_size, col_player_rewards, nash_equilibria_locations)
        self.nash_equilibria_locations = nash_equilibria_locations
        self.pareto_optimal_locations = pareto_optimal_locations
        self.verbose = False

    def get_nash_equilibria_locations(self):
        nash_equilibria_locations = []
        for i in range(len(self.game_table)):
            for j in range(len(self.game_table[i])):
                if self.is_nash_equilibrium(self.game_table[i][j]):
                    nash_equilibria_locations.append((i, j))
        return tuple(nash_equilibria_locations)

    def is_nash_equilibrium(self, cell: tuple[int, int]):
        for i in range(len(self.game_table)):
            if self.game_table[i][cell[1]][0] > self.game_table[cell[0]][cell[1]][0]:
                return False
        for j in range(len(self.game_table[cell[0]])):
            if self.game_table[cell[0]][j][1] > self.game_table[cell[0]][cell[1]][1]:
                return False
        return True

    def get_pareto_optimal_locations(self):
        pareto_optimal_locations = []
        for row in self.game_table:
            for cell in row:
                if self.is_pareto_optimal(cell):
                    pareto_optimal_locations.append(cell)
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
        print(f"Row Player Strongly Dominated Strategy: {self.row_player.strongly_dominated_strategy}")
        print(f"Column Player Strongly Dominated Strategy: {self.column_player.strongly_dominated_strategy}")
        print(f"Row Player Weakly Dominated Strategies: {self.row_player.weakly_dominated_strategies}")
        print(f"Column Player Weakly Dominated Strategies: {self.column_player.weakly_dominated_strategies}")
        self.run_games()

    def run_games(self):
        for row_strategy_title in self.row_player.strategies:
            for col_strategy_title in self.column_player.strategies:
                print()
                print(f"{row_strategy_title} vs {col_strategy_title}")
                self.run_game(row_strategy_title, col_strategy_title)

    def run_game(self, row_strategy_title: str, column_strategy_title: str):
        row_strategy = getattr(self.row_player, row_strategy_title)
        column_strategy = getattr(self.column_player, column_strategy_title)
        row = {
            "strategy": row_strategy,
            "index": self.row_player.index,
            "title": self.row_player.title
        }
        column = {
            "strategy": column_strategy,
            "index": self.column_player.index,
            "title": self.column_player.title
        }
        print("Row Player goes first")
        self.round(row, column)
        print()
        print("Column Player goes first")
        self.round(column, row)

    def round(self, first_player, second_player):
        first_player_choice = first_player["strategy"](None)
        second_player_choice = second_player["strategy"](first_player_choice)
        player_rewards = self.game_table[first_player_choice][second_player_choice]
        first_player_reward = player_rewards[first_player["index"]]
        second_player_reward = player_rewards[second_player["index"]]
        first_player_title = first_player["title"]
        second_player_title = second_player["title"]
        print(f"{first_player_title} chose:", first_player_choice)
        print(f"{second_player_title} chose:", second_player_choice)
        print(f"{first_player_title} reward:", first_player_reward)
        print(f"{second_player_title} reward:", second_player_reward)

    def print_game_table(self):
        print(f"{self.game_title}:")
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
    Game.main()
