import math


def main():
    #function = lambda n : round(math.factorial(n) / math.e) if n != 0 else 1
    function = lambda n : math.factorial(n)
    left = 0
    right = 20
    down = 20
    print_differance_table(get_differance_table(function, left, right, down))


def get_differance_table(function, left=0, right=10, down=10):
    """
    :param function: lambda function that takes an integer and returns an integer. ex. lambda n : n ** 2
    :param left: how far to the left should be calculated relative to 0.
    :param right: how far to the right should be calculated relative to 0.
    :param down: how far down should be calculated relative to n.
    :return: a matrix that has the values of the forward differance of the function given. First row is n, the second f(n), after that you get derivatives of f(n)
    """
    right += 1
    down += 2
    differance_table = [[], []]
    for n in range(left, right):
        differance_table[0].append(n)
        differance_table[1].append(function(n))
    for i in range(2, down):
        differance_table.append([])
        for j in range(0, len(differance_table[i - 1]) - 1):
            differance_table[i].append(differance_table[i - 1][j + 1] - differance_table[i - 1][j])
    return differance_table


def print_differance_table(differance_table):
    max_len = max(max(max(max(len(str(item)) for row in differance_table for item in row), len("n")), len("f(n)")), len(f"Δ^({len(differance_table) + 1})f(n)"))
    count = -1
    for row in differance_table:
        if count == -1:
            item = "n"
        elif count == 0:
            item = "f(n)"
        else:
            item = f"Δ^({count})f(n)"
        print(f"{item:<{max_len}}", end="      ")
        for item in row:
            print(f"{item:<{max_len}}", end="")
        print()
        count += 1


if __name__ == '__main__':
    main()