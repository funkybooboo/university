# Nate Stott A02386053
# CS 5110 - Vicki Allan - Program3 Normal Form Games
# 10/10/2021


from random import randint, shuffle


def main():
    for game in range(1, 4):
        print()
        print("*" * 50)
        print(f"Game {game}:")
        game_table, row_player_size, column_player_size = get_game_table(f"../data/game_{game}.txt")
        if not game_table:
            continue
        print_table(game_table, "Game Table")

        row_player_strongly_dominated_strategy = get_row_player_strongly_dominated_strategy(game_table)
        column_player_strongly_dominated_strategy = get_column_player_strongly_dominated_strategy(game_table)
        row_player_weakly_dominated_strategies = get_row_player_weakly_dominated_strategies(game_table)
        column_player_weakly_dominated_strategies = get_column_player_weakly_dominated_strategies(game_table)

        row_player = {"strategy": None, "title": "row player", "index": 0, "size": row_player_size, "strongly_dominated_strategy": row_player_strongly_dominated_strategy,
                      "weakly_dominated_strategies": row_player_weakly_dominated_strategies, "nash_equilibrium": None}
        column_player = {"strategy": None, "title": "column player", "index": 1, "size": column_player_size, "strongly_dominated_strategy": column_player_strongly_dominated_strategy,
                         "weakly_dominated_strategies": column_player_weakly_dominated_strategies, "nash_equilibrium": None}
        run_games(game_table, row_player, column_player)


def run_games(game_table, row_player, column_player):
    strategies = {"minimax": minimax_strategy, "maximin": maximin_strategy, "mixed": mixed_strategy, "pure": pure_strategy}
    if strongly_dominated_strategy:
        strategies["strongly dominated"] = strongly_dominated_strategy
    if weakly_dominated_strategy:
        strategies["weakly dominated"] = weakly_dominated_strategy
    for row_player_strategy_title, row_player_strategy in strategies.items():
        for column_player_strategy_title, column_player_strategy in strategies.items():
            run_game(row_player, column_player, game_table, f"{row_player_strategy_title} vs {column_player_strategy_title}", row_player_strategy, column_player_strategy)


def run_game(row_player, column_player, game_table, title, row_player_strategy, column_player_strategy):
    print()
    print(f"Running game for {title}")
    row_player["strategy"] = row_player_strategy
    column_player["strategy"] = column_player_strategy
    play_game(game_table, row_player, column_player)


def get_row_player_strongly_dominated_strategy(game_table):
    if not game_table:
        return None
    options = get_row_player_options(game_table)
    if len(options) == 1:
        return 0
    return get_best_dominating_option(options)


def get_best_dominating_option(options):
    if not options:
        return None
    if len(options) == 1:
        return 0
    best_option = None

    current_option = 0
    next_option = 1

    while next_option < len(options):

        option1 = options[current_option]
        option2 = options[next_option]

        option1_count = 0
        option2_count = 0

        for i in range(len(option1)):
            if option1[i] > option2[i]:
                option1_count += 1
            elif option1[i] < option2[i]:
                option2_count += 1
            else:
                break
        if option1_count == len(option1):
            best_option = current_option
            next_option += 1
        elif option2_count == len(option2):
            current_option = next_option
            best_option = current_option
            next_option += 1
        else:
            best_option = None
            current_option += 1
            next_option += 1

    return best_option


def get_row_player_options(game_table):
    options = []
    for row in game_table:
        items = []
        for cell in row:
            items.append(cell[0])
        options.append(items)
    return options


def get_column_player_strongly_dominated_strategy(game_table):
    if not game_table:
        return None
    options = get_column_player_options(game_table)
    if len(options) == 1:
        return 0
    return get_best_dominating_option(options)


def get_column_player_options(game_table):
    options = []
    for i in range(len(game_table[0])):
        items = []
        for j in range(len(game_table)):
            items.append(game_table[j][i][1])
        options.append(items)
    return options


def get_row_player_weakly_dominated_strategies(game_table):
    if not game_table:
        return None
    row_player_weakly_dominated_strategies = []
    options = get_row_player_options(game_table)
    if len(options) == 1:
        return tuple(row_player_weakly_dominated_strategies)
    get_best_weakly_dominating_options(options, row_player_weakly_dominated_strategies)
    return tuple(row_player_weakly_dominated_strategies)


def get_best_weakly_dominating_options(options, weakly_dominated_strategies):
    if not options:
        return None
    if len(options) == 1:
        return 0

    current_option = 0
    next_option = 1

    while next_option < len(options):
        temp_weakly_dominated_strategies = []

        option1 = options[current_option]
        option2 = options[next_option]

        option1_count = 0
        option2_count = 0

        for i in range(len(option1)):
            if option1[i] > option2[i]:
                option1_count += 1
            elif option1[i] < option2[i]:
                option2_count += 1
            else:
                option1_count += 1
                option2_count += 1

        if option1_count == len(option1) and option2_count == len(option2):
            temp_weakly_dominated_strategies.append(current_option)
            temp_weakly_dominated_strategies.append(next_option)
            next_option += 1
        elif option1_count == len(option1) and option2_count < len(option2):

            temp_weakly_dominated_strategies.append(current_option)
            next_option += 1
        elif option1_count < len(option1) and option2_count == len(option2):

            temp_weakly_dominated_strategies.append(next_option)
            current_option = next_option
            next_option += 1
        else:
            current_option += 1
            next_option += 1

        weakly_dominated_strategies = get_new_weakly_dominated_strategies(options, temp_weakly_dominated_strategies, weakly_dominated_strategies)


def get_new_weakly_dominated_strategies(options, temp_weakly_dominated_strategies, weakly_dominated_strategies):
    new_weakly_dominated_strategies = []
    check_option = options[temp_weakly_dominated_strategies[0]]
    for stored_option_index in weakly_dominated_strategies:

        stored_option = options[stored_option_index]

        check_option_count = 0
        stored_option_count = 0

        for i in range(len(check_option)):
            if check_option[i] > stored_option[i]:
                check_option_count += 1
            elif check_option[i] < stored_option[i]:
                stored_option_count += 1
            else:
                check_option_count += 1
                stored_option_count += 1

        if check_option_count == len(check_option) and stored_option_count < len(stored_option):
            for option_index in temp_weakly_dominated_strategies:
                new_weakly_dominated_strategies.append(option_index)
        elif check_option_count < len(check_option) and stored_option_count == len(stored_option):
            new_weakly_dominated_strategies.append(stored_option_index)
        else:
            new_weakly_dominated_strategies.append(stored_option_index)
            for option_index in temp_weakly_dominated_strategies:
                new_weakly_dominated_strategies.append(option_index)
    weakly_dominated_strategies = new_weakly_dominated_strategies
    return weakly_dominated_strategies


def get_column_player_weakly_dominated_strategies(game_table):
    if not game_table:
        return None
    column_player_weakly_dominated_strategies = []
    options = get_row_player_options(game_table)
    if len(options) == 1:
        return tuple(column_player_weakly_dominated_strategies)
    get_best_weakly_dominating_options(options, column_player_weakly_dominated_strategies)
    return tuple(column_player_weakly_dominated_strategies)


def get_nash_equilibria_locations(game_table):
    nash_equilibria_locations = []

    return tuple(nash_equilibria_locations)


def get_pareto_optimal_locations(game_table):
    pareto_optimal_locations = []

    return tuple(pareto_optimal_locations)


def play_game(game_table, player_a, player_b):
    player_a_choice = player_a["strategy"](game_table, player_a, None)
    player_b_choice = player_b["strategy"](game_table, player_b, player_a_choice)
    player_a_title = player_a["title"]
    print(f"{player_a_title} chose:", player_a_choice)
    player_b_title = player_b["title"]
    print(f"{player_b_title} chose:", player_b_choice)
    player_rewards = game_table[player_a_choice][player_b_choice]
    player_a_reward = player_rewards[player_a["index"]]
    player_b_reward = player_rewards[player_b["index"]]
    print(f"{player_a_title} reward:", player_a_reward)
    print(f"{player_b_title} reward:", player_b_reward)


def minimax_strategy(game_table, player, last_choice):
    return 0


def maximin_strategy(game_table, player, last_choice):
    return 0


def mixed_strategy(game_table, player, last_choice):
    return 0


def pure_strategy(game_table, player, last_choice):
    return randint(0, player["size"] - 1)


def strongly_dominated_strategy(game_table, player, last_choice):
    return player["strongly_dominated_strategy"]


def weakly_dominated_strategy(game_table, player, last_choice):
    return shuffle(player["weakly_dominated_strategies"])[0]


def print_table(table, table_title):
    print(f"{table_title}:")
    max_cell_length = 0
    for i in range(len(table)):
        for j in range(len(table[i])):
            cell_length = len(str(table[i][j]).strip("(").strip(")"))
            if cell_length > max_cell_length:
                max_cell_length = cell_length
    max_cell_length += 2
    row_length = (max_cell_length + 3) * len(table[0]) + 1
    print("-" * row_length)
    spaces = " " * max_cell_length
    print(f"  |", end="")
    for i in range(len(table[0])):
        this_spaces = " " * (len(spaces) - len(str(i)))
        print(f"{this_spaces}{i} |", end="")
    print()
    print("-" * row_length)
    for i in range(len(table)):
        start = f"{i} |"
        print(start, end="")
        for j in range(len(table[i])):
            cell = str(table[i][j]).strip("(").strip(")")
            this_spaces = " " * (len(spaces) - len(cell))
            print(f"{this_spaces}{cell} |", end="")
        print()
        print("-" * row_length)
    print()


def get_game_table(file_path):
    data = get_info_from_game_file(file_path)
    if not data:
        return None
    row_player_size, col_player_size, row_player_rewards, col_player_rewards = data
    game_table = []
    for i in range(row_player_size):
        game_table.append([])
        for j in range(i * col_player_size, (i + 1) * col_player_size):
            game_table[i].append((row_player_rewards[j], col_player_rewards[j]))
        game_table[i] = tuple(game_table[i])
    return tuple(game_table), row_player_size, col_player_size


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
    main()
