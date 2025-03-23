from itertools import product
from sys import argv


def main(args):
    m = 5
    v = False
    if len(args) > 0 and args[0].isdigit():
        m = int(args[0])
    if len(args) > 1 and args[1].lower() == 't':
        v = True
    for n in range(m + 1):
        print(f"P_{n} = {p(n, v)}")


def p(n, verbose=False):
    characters = ['a', 'b', 'c', 'd', 'e']
    invalids = ['cd', 'ce', 'ed', 'ee']
    sequences = generate_sequences(n, characters)
    good, bad = filter_sequences(sequences, invalids)
    if verbose:
        print_sequences(sequences, good, bad)
    return len(good)


def print_sequences(sequences, good, bad):
    print("*" * 50)
    print("All")
    print(sequences)
    print(len(sequences))
    print()
    print("Bad")
    print(bad)
    print(len(bad))
    print()
    print("Good")
    print(good)
    print(len(good))


def filter_sequences(sequences, invalids):
    good_sequences = []
    bad_sequences = []
    for sequence in sequences:
        good = True
        for invalid in invalids:
            if invalid in sequence:
                good = False
                break
        if good:
            good_sequences.append(sequence)
        else:
            bad_sequences.append(sequence)
    return good_sequences, bad_sequences


def generate_sequences(n, characters):
    return [''.join(seq) for seq in product(characters, repeat=n)]


if __name__ == '__main__':
    main(argv[1:])
