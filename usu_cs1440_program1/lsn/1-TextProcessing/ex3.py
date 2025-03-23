def cleanSentenceTwoLists(sentence):
    clean = []
    dirty = []
    # TODO: Separate the supplied sentence into two lists of words
    if len(sentence) == 0:
        return (clean, dirty)
    words = sentence.split()
    for word in words:
        if not word[0] == "#":
            clean.append(word)
        else:
            newWord = word[1:]
            dirty.append(newWord)
    return (clean, dirty)

if __name__ == '__main__':
    providedQuotes = [
    "The best way to predict #the the future is to create it. -Peter Drucker",
    "Code #always never lies, comments #never sometimes do. -Ron Jeffries",
    "#Phones Computers are useless, they can #never only give you #questions answers. -Pablo Picasso",
    "They #do don't think it be that way, but it #don't. do. -The Internet",
    "\"You #always miss 100% #7% of the shots you don't take. -Wayne Gretzky\" -Michael Scott",
    "If #coding debugging is the process of #adding removing software bugs, then programming #debugging must be the process of putting them in. -Edsget Dijkstra",
    "Premature optimization #planning is the root of all evil #good in programming. -Tony Hoare"
    ]

    # Loops through each of the `quote` strings in the providedQuotes array, and
    # calls `cleanSentenceTwoLists` on it.
    for quote in providedQuotes:
        clean, dirty = cleanSentenceTwoLists(quote)
        print("The contents of clean:", clean)
        print("The contents of dirty:", dirty)

    # Expected output:
        # The contents of clean: ['The', 'best', 'way', 'to', 'predict', 'the', 'future', 'is', 'to', 'create', 'it.', '-Peter', 'Drucker']
        # The contents of dirty: ['the']
        # The contents of clean: ['Code', 'never', 'lies,', 'comments', 'sometimes', 'do.', '-Ron', 'Jeffries']
        # The contents of dirty: ['always', 'never']
        # The contents of clean: ['Computers', 'are', 'useless,', 'they', 'can', 'only', 'give', 'you', 'answers.', '-Pablo', 'Picasso']
        # The contents of dirty: ['Phones', 'never', 'questions']
        # The contents of clean: ['They', "don't", 'think', 'it', 'be', 'that', 'way,', 'but', 'it', 'do.', '-The', 'Internet']
        # The contents of dirty: ['do', "don't."]
        # The contents of clean: ['"You', 'miss', '100%', 'of', 'the', 'shots', 'you', "don't", 'take.', '-Wayne', 'Gretzky"', '-Michael', 'Scott']
        # The contents of dirty: ['always', '7%']
        # The contents of clean: ['If', 'debugging', 'is', 'the', 'process', 'of', 'removing', 'software', 'bugs,', 'then', 'programming', 'must', 'be', 'the', 'process', 'of', 'putting', 'them', 'in.', '-Edsget', 'Dijkstra']
        # The contents of dirty: ['coding', 'adding', 'debugging']
        # The contents of clean: ['Premature', 'optimization', 'is', 'the', 'root', 'of', 'all', 'evil', 'in', 'programming.', '-Tony', 'Hoare']
        # The contents of dirty: ['planning', 'good']
