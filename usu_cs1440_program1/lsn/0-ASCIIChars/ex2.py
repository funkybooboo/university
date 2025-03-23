def listOfASCIIInts(charList):
    lst = []
    # TODO: convert a list of characters into a list of ints based on the
    #   characters ASCII values
    if len(charList) == 0:
        return lst
    for char in charList:
        lst.append(ord(char))
    return lst


if __name__ == '__main__':
    # Should produce: [65, 66, 99, 49, 45, 95, 126, 32, 122, 89, 120]
    provided = ["A", "B", "c", "1", "-", "_", "~", " ", "z", "Y", "x"]

    # Should produce: [87, 105, 108, 108, 32, 121, 111, 117, 32, 112, 97, 115, 115, 32, 116, 104, 105, 115, 32, 116, 101, 115, 116, 32, 116, 111, 111, 63]
    provided2 = "Will you pass this test too?"

    print(listOfASCIIInts(provided))
    print(listOfASCIIInts(provided2))
