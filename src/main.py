def main():
    for game in range(1, 4):
        print(f"Game {game}:")
        game_table = get_game_table(f"../data/game_{game}.txt")
        if not game_table:
            return
        print_game_table(game_table)


def print_game_table(game_table):
    print("Game table:")
    for i in range(len(game_table)):
        print(game_table[i])


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
    return game_table


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
