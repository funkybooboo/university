def scoreWord(word):
    score = 0
    # TODO: Score the word
    # HINT: Try out "a".isalpha() and "!".isalpha() in the REPL! `help(str.isalpha)` may also come in handy :)

    if len(word) == 0:
        return 0
    for char in word:
        if char.isalpha():
            score += getScoreFromChar(char)
        else:
            score += 0

    return score

def getScoreFromChar(char):
    count1 = 65
    count2 = 97
    score = 0
    while(True):
        if count1 > 90 or count2 > 122 or score > 25:
            break
        if ord(char) == count1 or ord(char) == count2:
            return score
        count1 += 1
        count2 += 1
        score += 1

if __name__ == '__main__':
    provided = [
        "One",
        "oNE",
        "supercalifragilisticexpialidocious",
        "t",
        "aAaA",
        "Zap!",
        "Tr!ck3d y4!",
        "G0t it!"
    ]

    # For each word in the provided list, give the word to the function score word and print some fancy formatted output
    for word in provided:
        print(f"The score of {word} is: {scoreWord(word)}")

    # Should Output:
        # The score of One is: 31
        # The score of oNE is: 31
        # The score of supercalifragilisticexpialidocious is: 345
        # The score of t is: 19
        # The score of aAaA is: 0
        # The score of Zap! is: 40
        # The score of Tr!ck3d y4! is: 75
        # The score of G0t it! is: 52