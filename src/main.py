# Nate Stott A02386053
# CS 5110 - Vicki Allan - Program3 Normal Form Games
# 10/10/2021


from random import randint, shuffle


class Player:

    def __init__(self, title, index, number_of_options, rewards, nash_equilibria_locations):
        self.title = title
        self.index = index
        self.number_of_options = number_of_options
        self.rewards = rewards
        self.options = self.__get_options()
        self.nash_equilibria = self.__get_nash_equilibria(nash_equilibria_locations)
        self.strongly_dominated_strategy = self.__get_strongly_dominated_strategy()
        self.weakly_dominated_strategies = self.__get_weakly_dominated_strategies()

        self.strategies = [
            "minimax",
            "maximin",
            "mixed",
            "pure",
            "nash equilibrium",
            "strongly dominated",
            "weakly dominated"
        ]

    def __get_strongly_dominated_strategy(self):
        pass

    def __get_weakly_dominated_strategies(self):
        pass

    def __get_options(self):
        pass

    def __get_nash_equilibria(self, nash_equilibria_locations):
        pass

    def minimax_strategy(self, last_choice):
        return 0

    def maximin_strategy(self, last_choice):
        return 0

    def mixed_strategy(self, last_choice):
        return 0

    def pure_strategy(self, last_choice):
        if last_choice is not None:
            return randint(0, self.number_of_options - 1)
        return 0

    def nash_equilibrium_strategy(self, last_choice):
        return self.nash_equilibria[self.index]

    def strongly_dominated_strategy(self, last_choice):
        return self.strongly_dominated_strategy

    def weakly_dominated_strategy(self, last_choice):
        return shuffle(self.weakly_dominated_strategies)[0]


class Game:

    @staticmethod
    def main():
        for game_number in range(1, 4):
            print()
            print("*" * 50)
            print(f"Game {game_number}:")
            game = Game(game_number)
            game.play()

    def __init__(self, game):

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

        return tuple(nash_equilibria_locations)

    def get_pareto_optimal_locations(self):
        pareto_optimal_locations = []
        for row in self.game_table:
            for cell in row:
                if self.is_pareto_optimal(cell):
                    pareto_optimal_locations.append(cell)
        return tuple(pareto_optimal_locations)

    def is_pareto_optimal(self, cell):
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

    def run_game(self, row_strategy_title, column_strategy_title):
        row_strategy = getattr(self.row_player, f"{row_strategy_title}_strategy")
        column_strategy = getattr(self.column_player, f"{column_strategy_title}_strategy")
        row_player_choice = row_strategy(None)
        column_player_choice = column_strategy(row_player_choice)
        player_rewards = self.game_table[row_player_choice][column_player_choice]
        row_player_reward = player_rewards[self.row_player.index]
        column_player_reward = player_rewards[self.column_player.index]
        print(f"{self.row_player.title} chose:", row_player_choice)
        print(f"{self.row_player.title} chose:", column_player_choice)
        print(f"{self.row_player.title} reward:", row_player_reward)
        print(f"{self.column_player.title} reward:", column_player_reward)

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

    def get_game_table(self, file_path):
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
    def get_info_from_game_file(file_path):
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
