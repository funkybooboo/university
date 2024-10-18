#include "WordPredictor/WordPredictor.hpp"
#include "WordTree/WordTree.hpp"

#include <algorithm>
#include <fstream>
#include <iterator>
#include <memory>
#include <sstream>
#include <string>

/**
 * @brief Reads a dictionary file and populates a WordTree with valid words.
 *
 * This function processes each line in the provided file, checks if the
 * word contains only alphabetic characters, converts it to lowercase,
 * and adds it to the WordTree.
 *
 * @param filename The path to the dictionary file.
 * @return A shared pointer to the populated WordTree.
 */
std::shared_ptr<WordTree> readDictionary(const std::string& filename)
{
    // Create a shared pointer for the WordTree
    auto wordTree = std::make_shared<WordTree>();

    // Open the dictionary file for reading
    auto inFile = std::ifstream(filename, std::ios::in);

    // Process the file until the end
    while (!inFile.eof())
    {
        std::string word;
        std::getline(inFile, word); // Read a line from the file

        // Remove carriage return character if it exists (for compatibility)
        if (!word.empty() && word[word.size() - 1] == '\r')
        {
            word.erase(word.end() - 1);
        }

        // Check if the word consists of alphabetic characters
        // std::isalpha requires an unsigned char to avoid exceptions for negative values
        if (std::ranges::all_of(word, [](const unsigned char c)
                                {
                                    return std::isalpha(c);
                                }))
        {
            // Convert the word to lowercase before adding it to the tree
            std::ranges::transform(word, word.begin(), [](const char c)
                                   {
                                       return static_cast<char>(std::tolower(c));
                                   });
            // Add the valid lowercase word to the WordTree
            wordTree->add(word);
        }
    }

    return wordTree; // Return the populated WordTree
}

/**
 * @brief The main function of the program.
 *
 * This function initializes the WordTree with words from the dictionary
 * and creates a WordPredictor to handle user input and predictions.
 *
 * @return Exit status of the program.
 */
int main()
{
    // Read the dictionary and create the WordTree
    const std::shared_ptr<WordTree> wordTree = readDictionary("../data/dictionary.txt");

    // Create a WordPredictor instance with the populated WordTree
    const WordPredictor predictor(wordTree);

    // Render the predictor interface
    predictor.render();
}
