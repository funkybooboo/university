#include "WordTree/WordTree.hpp"
#include "rlutil.h"

#include <algorithm>
#include <fstream>
#include <iterator>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

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

void renderConsole(const std::shared_ptr<WordTree>& wordTree)
{
    std::string input;
    std::string lastWord;
    std::vector<std::string> predictions;

    rlutil::cls();

    while (true)
    {
        // Display the current input line
        rlutil::locate(1, 1);
        std::cout << "Input: " << input << std::endl;

        // Split the input to get the last word
        if (auto words = split(input, ' '); !words.empty())
        {
            lastWord = words.back();
        }
        else
        {
            lastWord.clear();
        }

        // Get predictions based on the last word
        if (!lastWord.empty())
        {
            predictions = wordTree->predict(lastWord, rlutil::trows() - 4); // Adjust for input line and title
        }

        // Display predictions
        rlutil::locate(1, 3); // Move to the prediction line
        std::cout << "--- prediction ---" << std::endl;
        for (const auto& word : predictions)
        {
            std::cout << word << std::endl;
        }

        // Capture user input
        const int key = rlutil::getkey();
        if (key == 27) // Escape key to exit
        {
            break;
        }
        if (key == rlutil::KEY_BACKSPACE)
        {
            if (!input.empty())
            {
                input.pop_back(); // Remove last character
            }
        }
        else if (std::isprint(key))
        {
            input += static_cast<char>(key); // Append character to input
        }
    }
}

int main()
{
    const std::shared_ptr<WordTree> wordTree = readDictionary("../data/dictionary.txt");
    renderConsole(wordTree);
    return 0;
}
