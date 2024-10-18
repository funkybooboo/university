#include "WordTree/WordTree.hpp"
#include "rlutil.h"

#include <algorithm>
#include <fstream>
#include <iterator>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

std::shared_ptr<WordTree> readDictionary(const std::string& filename)
{
    auto wordTree = std::make_shared<WordTree>();
    auto inFile = std::ifstream(filename, std::ios::in);

    while (!inFile.eof())
    {
        std::string word;
        std::getline(inFile, word);
        // Need to consume the carriage return character for some systems, if it exists
        if (!word.empty() && word[word.size() - 1] == '\r')
        {
            word.erase(word.end() - 1);
        }
        // Keep only if everything is an alphabetic character -- Have to send isalpha an unsigned char or
        // it will throw exception on negative values; e.g., characters with accent marks.
        if (std::ranges::all_of(word, [](const unsigned char c)
                                {
                                    return std::isalpha(c);
                                }))
        {
            std::ranges::transform(word, word.begin(), [](const char c)
                                   {
                                       return static_cast<char>(std::tolower(c));
                                   });
            wordTree->add(word);
        }
    }

    return wordTree;
}

template <typename Out>
void split(const std::string& s, const char delim, Out result)
{
    std::istringstream iss(s);
    std::string item;
    while (std::getline(iss, item, delim))
    {
        *result++ = item;
    }
}

std::vector<std::string> split(const std::string& s, const char delim)
{
    std::vector<std::string> elems;
    split(s, delim, std::back_inserter(elems));
    return elems;
}

void displayUserInput(const std::string& userInput)
{
    rlutil::locate(1, 1);   // Move cursor to the start
    std::cout << userInput; // Display current input
}

std::string getLastWord(const std::string& userInput)
{
    if (auto words = split(userInput, ' '); !words.empty())
    {
        return words.back(); // Get the most recent word
    }
    return "";
}

void displayPredictions(const std::vector<std::string>& predictions)
{
    rlutil::locate(1, 3); // Move cursor to the prediction header line
    std::cout << "--- prediction ---" << std::endl;

    rlutil::locate(1, 4); // Move to prediction area
    for (const auto& pred : predictions)
    {
        std::cout << pred << std::endl;
    }
}

void handleUserInput(std::string& userInput)
{
    if (const int key = rlutil::getkey(); key == rlutil::KEY_BACKSPACE)
    { // Handle backspace
        if (!userInput.empty())
        {
            userInput.pop_back(); // Remove last character
        }
    }
    else if (isprint(static_cast<char>(key)) || key == ' ')
    {                                        // Check if the key is printable or space
        userInput += static_cast<char>(key); // Append character to input
    }
}

[[noreturn]] void render(const std::shared_ptr<WordTree>& wordTree)
{
    std::string userInput;

    while (true)
    {
        rlutil::cls(); // Clear the console
        displayUserInput(userInput);

        std::string lastWord = getLastWord(userInput);
        auto predictions = wordTree->predict(lastWord, rlutil::trows() - 4); // Get predictions

        displayPredictions(predictions);

        handleUserInput(userInput);
    }
}

int main()
{
    const std::shared_ptr<WordTree> wordTree = readDictionary("../data/dictionary.txt");
    render(wordTree);
}
