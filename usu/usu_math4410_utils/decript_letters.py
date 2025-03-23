convert = {
    "set1": {
        0: 'A',
        1: 'B',
        2: 'C',
        3: 'D',
        4: 'E',
        5: 'F',
        6: 'G',
        7: 'H',
        8: 'I',
        9: 'J'
    },
    "set2": {
        0: 'K',
        1: 'L',
        2: 'M',
        3: 'N',
        4: 'O',
        5: 'P',
        6: 'Q',
        7: 'R',
        8: 'S',
        9: 'T'
    },
    "set3": {
        0: 'U',
        1: 'V',
        2: 'W',
        3: 'X',
        4: 'Y',
        5: 'Z'
    },
    "set4": {
        1: ' '
    }

}

numbers = [
    847,
    741,
    547,
    197,
    418,
    324,
    314,
    384,
    324,
]

possibles = []

for number in numbers:
    number = str(number)
    for digit in number:
        possible = []
        for i in range(1, 5):
            for key, value in convert[f"set{i}"].items():
                if int(digit) == key:
                    possible.append(value)
        possibles.append(possible)

for p in possibles:
    print(p)
