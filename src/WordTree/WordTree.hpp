#pragma once

#include <string>
#include <vector>
#include <cstdint>

class WordTree {

public:
    /*
     - Adds a new word to the tree.  If the word already exists, it doesn't result in a duplication.  If the tree is written correctly, no special code for handling a duplicate is necessary.
     - Convert the word to lowercase (std::tolower) and add only as lowercase.
     - If the word has length of 0, do not add the word.
     - If the word has any non-alphabetic characters (std::isalpha), do not add the word.
     */
    void add(std::string word);

    /*
     - Searches the tree to see if the word is found.  If found, true is returned, false otherwise.
     - Convert the word to lowercase (std::tolower) and search only as lowercase.
     - If the word length of 0, reject the search, returning false.
     - If the word has any non-alphabetic characters, reject the search, returning false.
     */
    bool find(std::string word);

    /*
     - Given the partial (or possibly complete word) word, returns up to howMany predicted words.  The prediction must be a breadth-first-search prediction of the next possible words.  This requires a breath-first search of the tree, after the node where the partial word ends.
     - Here is a wiki link on how to perform a breadth first search: https://en.wikipedia.org/wiki/Breadth-first_search (Links to an external site.)
     - Convert the word to lowercase (std::tolower) and predict only as lowercase.
     - If the word has length of 0, reject the prediction, returning an empty vector.
     - If the word has any non-alphabetic characters, reject the prediction, returning an empty vector.
    */
    std::vector<std::string> predict(std::string partial, std::uint8_t howMany);

    /*
      - Returns a count of the number of words in the tree.  This must traverse the tree to count the number of words, you cannot store a data member to track the number of words (because I want you to practice traversing a tree).
     */
    std::size_t size();
};


