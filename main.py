
def main():
    print_differance_table(get_differance_table(lambda n : n ** 7, 11))


def get_differance_table(function, columns):
    differance_table = [[], []]
    for n in range(0, columns):
        differance_table[0].append(n)
        differance_table[1].append(function(n))
    count = 0
    for i in range(2, columns - 1):
        differance_table.append([])
        for n in range(0, columns - 1 - count):
            differance_table[i].append(differance_table[i - 1][n + 1] - differance_table[i - 1][n])
        count += 1
    return differance_table

def print_differance_table(differance_table):
    max_len = max(len(str(item)) for row in differance_table for item in row)
    for row in differance_table:
        for item in row:
            print(f"{item:<{max_len}}", end="")
        print()


if __name__ == '__main__':
    main()