
def main():
    differance_table = [[], []]
    for n in range(0, 8):
        differance_table[0].append(n)
        differance_table[1].append(n ** 4)
    count = 0
    for i in range(2, 7):
        differance_table.append([])
        for n in range(0, 7 - count):
            differance_table[i].append(differance_table[i-1][n+1] - differance_table[i-1][n])
        count += 1
    for row in differance_table:
        print(row)


if __name__ == '__main__':
    main()