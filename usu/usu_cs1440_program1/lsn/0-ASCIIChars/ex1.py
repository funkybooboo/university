def listOfChars(intList):
    lst = []
    # TODO: Take each integer from intList and add corresponding `chr`'d characters to `lst`
    if len(intList) == 0:
        return lst
    for i in intList:
        lst.append(chr(i))
    return lst


if __name__ == '__main__':
    provided = [
        65,
        32,
        115,
        104,
        111,
        114,
        116,
        32,
        115,
        101,
        110,
        116,
        101,
        110,
        99,
        101,
        46
    ]

    # Returns a list of strings to result
    result = listOfChars(provided)

    # The following block of code turns the list `result` into a string `resultStr`
    # Do not modify!
    resultStr = ""
    for char in result:
        resultStr += char
    print(resultStr)
    # With the provided list above, prints out "A short sentence."